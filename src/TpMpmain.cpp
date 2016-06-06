#include <iostream>
#include <string>
#include <sstream>
#include <mathlink.h>
#include <unistd.h>

#include <yaml-cpp/yaml.h>
#include <fstream>
#include <chrono>
#include <ctime>

#include <sqlite3.h>

#include <boost/filesystem.hpp>

#include "types.hpp"
#include "graph.hpp"
#include "tadpole.hpp"
#include "sigma.hpp"

using namespace boost::filesystem;



const bool overwrite_db = true;

const char* sql_create = 
  "CREATE TABLE DIAGRAMS("  
  "ID INT PRIMARY        KEY      NOT NULL," 
  "L                     INT      NOT NULL," 
  "P                     INT      NOT NULL,"
  // "V                     INT      NOT NULL," 
  "FACTOR                INT      NOT NULL," 
  "PROPS                 TEXT     NOT NULL," 
  "LEGS                  TEXT     NOT NULL," 
  "VERTS                 TEXT     NOT NULL);";

// const char* sql_create = 
//   "CREATE TABLE DIAGRAMS("  
//   "ID INT PRIMARY        KEY      NOT NULL," 
//   "LOOPS                 INT      NOT NULL);";


// Make string in single quotes
std::string quoted(std::string s)
{
  std::stringstream qs;
  qs << "'" << s << "'";
  return qs.str();
}





class Timer
{
  std::chrono::high_resolution_clock::time_point ts;
public:
  Timer()
  {
    reset();
  }
  void reset()
  {
    ts = std::chrono::high_resolution_clock::now();    
  }  
  int get_ms()
  {
    return std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::high_resolution_clock::now() - ts).count();
  }
  float get_s()
  {
    return std::chrono::duration_cast<std::chrono::milliseconds>(std::chrono::high_resolution_clock::now() - ts).count()/1000.;
  }
};

// Print string in Mathematica
void mprint(const std::string& s)
{
  std::stringstream prntmsg;
  prntmsg << "WriteString[\"stdout\",\"" << s << "\"]";
  std::string temp = prntmsg.str();

  MLEvaluateString(stdlink, const_cast<char*>(temp.c_str()));
}


// Print string with endline
void mprintln(const std::string& s)
{
  std::stringstream prntmsg;
  prntmsg << "WriteString[\"stdout\",\"" << s << "\n\"]";
  std::string temp = prntmsg.str();

  MLEvaluateString(stdlink, const_cast<char*>(temp.c_str()));
}

void message(const std::string& mtag)
{
  std::stringstream ss;
  ss << "Message[" << mtag << "]";
  MLEvaluateString(stdlink, const_cast<char*>(ss.str().c_str()));
}

void message(const std::string& mtag, const std::string& param1)
{
  std::stringstream ss;
  ss << "Message[" << mtag << "," << param1 << "]";
  MLEvaluateString(stdlink, const_cast<char*>(ss.str().c_str()));
}

void message(const std::string& mtag, const std::string& param1, const std::string& param2)
{
  std::stringstream ss;
  ss << "Message[" << mtag << "," << param1 << "," << param2 << "]";
  MLEvaluateString(stdlink, const_cast<char*>(ss.str().c_str()));
}




static int callback(void *data, int argc, char **argv, char **azColName){
  int i;
  fprintf(stderr, "%s: ", (const char*)data);
  for(i=0; i<argc; i++){
    printf("%s = %s\n", azColName[i], argv[i] ? argv[i] : "NULL");
  }
  printf("\n");
  return 0;
}

// Call back with map
static int findSubstrings(void *data, int argc, char **argv, char **azColName)
{

  typedef std::map<size_t, std::vector<size_t> > fMap;
  int i;
  // fprintf(stderr, "%s: ", (const char*)data);
  
  fMap * mp = static_cast<fMap *>(data);
  
  // fMap->
  for(i=0; i<argc; i++)
    {
      printf("%s = %s\n", azColName[i], argv[i] ? argv[i] : "NULL");
    }
  printf("\n");
  return 0;
}




class QgrafSQL
{
  std::shared_ptr<sqlite3> db;
  sqlite3* dbPtr = NULL;
  
  bool statusOK = true;
public:
  bool ok()
  {
    return statusOK;
  }

  QgrafSQL()
  {
  }
  QgrafSQL(const std::string& dbname, bool overwritedb)
  {

    int flags = 0;
    if(overwritedb)
      flags = SQLITE_OPEN_READWRITE | SQLITE_OPEN_CREATE;
    else
      flags = SQLITE_OPEN_READWRITE;
    
    if( sqlite3_open_v2(dbname.c_str(), &dbPtr, flags, NULL) )
      {
        mprintln(sqlite3_errmsg(dbPtr));
        statusOK = false;
      }
    db.reset(dbPtr, sqlite3_close);
  }
  
  // Creating tables
  bool create()
  {
    if(statusOK)
      {
        char *err = 0;    
        if (sqlite3_exec(dbPtr, sql_create, 0, 0, &err))
          {
            fprintf(stderr, "Error SQL %sn", err);
            mprintln(err);
            sqlite3_free(err);
            return false;
          }
        else
          return true;
      }
    else
      return false;
  }

private:

  bool insertSingleDia(const DiagramRecord& dia)
  {
    sqlite3_stmt *insertStmt;
    // cout << "Creating Insert Statement" << endl;
    
    
    std::stringstream insertQuery;
    insertQuery << "INSERT INTO DIAGRAMS (ID,L,P,FACTOR,PROPS,LEGS,VERTS)"
      " VALUES (" 
                << dia.id << ", " 
                << dia.loops << ", " 
                << dia.props.size() << ", " 
                << dia.factor << ", " 
                << quoted(dia.propStr()) << ", " 
                << quoted(dia.legStr()) << ", " 
                << quoted("vertsxx") << ");";
    


    sqlite3_prepare(dbPtr, insertQuery.str().c_str(), insertQuery.str().size(), &insertStmt, NULL);
    // cout << "Stepping Insert Statement" << endl;
    if (sqlite3_step(insertStmt) != SQLITE_DONE) 
      mprintln("Didn't Insert Item!");
    return true;
  }

  // stmt - compiled SQL request
  bool insertSingleDiaBind(const DiagramRecord& dia, sqlite3_stmt *stmt)
  {
    
    int rc = sqlite3_bind_int(stmt, 1, dia.id); 
    rc = sqlite3_bind_int(stmt, 2, dia.loops); 
    rc = sqlite3_bind_int(stmt, 3, dia.props.size()); 
    rc = sqlite3_bind_int(stmt, 4, dia.factor); 
    rc = sqlite3_bind_text(stmt, 5, dia.propStr().c_str(), -1, SQLITE_TRANSIENT); 
    rc = sqlite3_bind_text(stmt, 6, dia.legStr().c_str(), -1, SQLITE_TRANSIENT); 
    rc = sqlite3_bind_text(stmt, 7, "vertsxx", -1, SQLITE_TRANSIENT); 


    sqlite3_step(stmt);
    
    sqlite3_clear_bindings(stmt);
    sqlite3_reset(stmt);

    // sqlite3_stmt *insertStmt;
    // // cout << "Creating Insert Statement" << endl;
    
    
    // std::stringstream insertQuery;
    // insertQuery << "INSERT INTO DIAGRAMS (ID,L,P,FACTOR,PROPS,LEGS,VERTS)"
    //   " VALUES (" 
    //             << dia.id << ", " 
    //             << dia.loops << ", " 
    //             << dia.props.size() << ", " 
    //             << dia.factor << ", " 
    //             << quoted(dia.propStr()) << ", " 
    //             << quoted(dia.legStr()) << ", " 
    //             << quoted("vertsxx") << ");";
    


    // sqlite3_prepare(dbPtr, insertQuery.str().c_str(), insertQuery.str().size(), &insertStmt, NULL);
    // // cout << "Stepping Insert Statement" << endl;
    // if (sqlite3_step(insertStmt) != SQLITE_DONE) 
    //   mprintln("Didn't Insert Item!");
    return true;
  }

public:

  bool insert(const std::vector<YAML::Node>& diagrams)
  {
    
    sqlite3_exec(dbPtr, "BEGIN TRANSACTION;", NULL, NULL, NULL);
    
    sqlite3_stmt *stmt;
    int rc = sqlite3_prepare_v2(dbPtr, "INSERT INTO DIAGRAMS (ID,L,P,FACTOR,PROPS,LEGS,VERTS) VALUES (?, ?, ?, ?, ?, ?, ?)", -1, &stmt, NULL);
    if (rc != SQLITE_OK)
      throw std::string(sqlite3_errmsg(dbPtr));
    
    
    for(std::vector<YAML::Node>::const_iterator dit = diagrams.begin(); 
        dit != diagrams.end(); ++dit)
      {
        DiagramRecord diagram_record(*dit);
        

        if(!insertSingleDiaBind(diagram_record, stmt)) return false;
      }

    sqlite3_exec(dbPtr, "END TRANSACTION;", NULL, NULL, NULL);

    return true;
  }

  void list()
  {
    char *zErrMsg = 0;
    int rc;
    // char *sql;
    const char* data = "Callback function called";

    /* Create SQL statement */
    // sql = "SELECT * from DIAGRAMS";
    const char* sql = "SELECT * FROM DIAGRAMS WHERE ID=5";


    /* Execute SQL statement */
    rc = sqlite3_exec(dbPtr, sql, callback, (void*)data, &zErrMsg);
    if( rc != SQLITE_OK ){
      fprintf(stderr, "SQL error: %s\n", zErrMsg);
      sqlite3_free(zErrMsg);
    }else{
      fprintf(stdout, "Operation done successfully\n");
    }
  }

  void like()
  {
    char *zErrMsg = 0;
    int rc;
    // char *sql;
    const char* data = "Callback function called";
    
    /* Create SQL statement */
    // sql = "SELECT * from DIAGRAMS";
    const char* sql = "SELECT PROPS FROM DIAGRAMS WHERE PROPS LIKE \'%[gt]%\'";
    

    /* Execute SQL statement */
    rc = sqlite3_exec(dbPtr, sql, callback, (void*)data, &zErrMsg);
    if( rc != SQLITE_OK )
      {
        fprintf(stderr, "SQL error: %s\n", zErrMsg);
        sqlite3_free(zErrMsg);
      }
    else
      {
        fprintf(stdout, "Operation done successfully\n");
      }
  }


  void find(size_t dianum)
  {
    char *zErrMsg = 0;
    int rc;
    char *sql;
    const char* data = "Callback function called";

    /* Create SQL statement */
    // sql = "SELECT * from DIAGRAMS";
    // sql = "SELECT * FROM DIAGRAMS WHERE ID=5";

    std::stringstream ss;
    ss << "SELECT * FROM DIAGRAMS WHERE ID=" << dianum; 
    
    /* Execute SQL statement */
    rc = sqlite3_exec(dbPtr, ss.str().c_str(), callback, (void*)data, &zErrMsg);
    if( rc != SQLITE_OK )
      {
        fprintf(stderr, "SQL error: %s\n", zErrMsg);
        sqlite3_free(zErrMsg);
      }
    else
      {
        fprintf(stdout, "Operation done successfully\n");
      }
  }

  bool select(int id, DiagramRecord& dr)
  {
    sqlite3_stmt *stmt;
    int rc = sqlite3_prepare_v2(dbPtr, "SELECT L,P,FACTOR,PROPS,LEGS,VERTS"
                                " FROM DIAGRAMS"
                                " WHERE ID = ?", -1, &stmt, NULL);
    if (rc != SQLITE_OK)
      throw std::string(sqlite3_errmsg(dbPtr));
    
    rc = sqlite3_bind_int(stmt, 1, id);    // Using parameters ("?") is not
    if (rc != SQLITE_OK) {                 // really necessary, but recommended
      std::string errmsg(sqlite3_errmsg(dbPtr)); // (especially for strings) to avoid
      sqlite3_finalize(stmt);            // formatting problems and SQL
      throw errmsg;                      // injection attacks.
    }
    
    rc = sqlite3_step(stmt);
    if (rc != SQLITE_ROW && rc != SQLITE_DONE) {
      std::string errmsg(sqlite3_errmsg(dbPtr));
      sqlite3_finalize(stmt);
      throw errmsg;
    }
    if (rc == SQLITE_DONE) {
      sqlite3_finalize(stmt);
      return false;
      // throw std::string("customer not found");
    }
    
    std::cout << "Loops  " << sqlite3_column_int(stmt, 0) << std::endl;
    std::cout << "Props num" << sqlite3_column_int(stmt, 1) << std::endl;
    std::cout << "Factor " << sqlite3_column_int(stmt, 2) << std::endl;
    std::cout << "Props " << sqlite3_column_text(stmt, 3) << std::endl;
    dr.id     = id;
    dr.loops  = sqlite3_column_int(stmt, 0);
    dr.factor = sqlite3_column_int(stmt, 2);
    
    dr.propStr(reinterpret_cast<const char*>(sqlite3_column_text(stmt, 3)));
    dr.legStr(reinterpret_cast<const char*>(sqlite3_column_text(stmt, 4)));
    // this->id         = id;
    // this->first_name = string(sqlite3_column_text(stmt, 0));
    // this->last_name  = string(sqlite3_column_text(stmt, 1));
    // this->age        =        sqlite3_column_int(stmt, 2);
    
    sqlite3_finalize(stmt);
    return true;
  }

  // return map of fields (f) multiplicities (fm)
  // 1 -> {1,3,5,...}
  // 2 -> {}
  // 3 -> {2,4,...}
  // ...
  bool findField(const std::string& f, std::map<size_t, std::set<size_t> >& fm)
  {
    mprint("Find field ");
    mprint(f);
    mprint("\n");
    
    std::stringstream likeQuery;
    likeQuery << "SELECT ID,PROPS FROM DIAGRAMS WHERE PROPS LIKE "
              << "\"%["
              << f 
              << "]%\"";
    sqlite3_stmt *stmt;
    int rc = sqlite3_prepare_v2(dbPtr, likeQuery.str().c_str(), -1, &stmt, NULL);
    if (rc != SQLITE_OK)
      throw std::string(sqlite3_errmsg(dbPtr));
    
    // Field substring to find
    std::stringstream ftf;
    ftf << "[" << f << "]";

    do
      {
        rc = sqlite3_step(stmt);
        if (rc != SQLITE_ROW && rc != SQLITE_DONE) 
          {
            std::string errmsg(sqlite3_errmsg(dbPtr));
            sqlite3_finalize(stmt);
            throw errmsg;
          }

        if(rc != SQLITE_DONE)   // When there is nothing to read
          {
            int id = sqlite3_column_int(stmt, 0);
            std::string propstr = (reinterpret_cast<const char*>(sqlite3_column_text(stmt, 1)));
            
            int field_occurrences = 0;
            std::string::size_type start = 0;
            
            while ((start = propstr.find(ftf.str(), start)) != std::string::npos) 
              {
                ++field_occurrences;
                start += ftf.str().length(); 
              }
            
            
            std::cout << "ID   :  " << sqlite3_column_int(stmt, 0) << std::endl;
            std::cout << "PROPS:  " << sqlite3_column_text(stmt, 1) << std::endl;
            std::cout << "MULT :  " << field_occurrences << std::endl;
            
            std::map<size_t, std::set<size_t> >::iterator vit = fm.find(field_occurrences);
            if (vit != fm.end())
              vit->second.insert(id);
            else
              {
                std::set<size_t> mult;
                mult.insert(id);
                fm[field_occurrences] = mult;
              }
          }
      }
    while(rc == SQLITE_ROW );   // When we have ROW to read

    if(rc == SQLITE_DONE) 
      {
        std::cout << "In done" << std::endl;
        sqlite3_finalize(stmt);
        return true;
      }
    else
      return false;
  }

  bool findFieldType(const std::string& f, std::map<size_t, std::set<size_t> >& fm)
  {
    std::stringstream likeQuery;
    likeQuery << "SELECT ID,PROPS FROM DIAGRAMS WHERE PROPS LIKE "
              << "\'%"
              << f 
              << "[%\'";
    sqlite3_stmt *stmt;
    int rc = sqlite3_prepare_v2(dbPtr, likeQuery.str().c_str(), -1, &stmt, NULL);
    if (rc != SQLITE_OK)
      throw std::string(sqlite3_errmsg(dbPtr));
    
    // Field substring to find
    std::stringstream ftf;
    ftf << f << "[";

    do
      {
        rc = sqlite3_step(stmt);
        if (rc != SQLITE_ROW && rc != SQLITE_DONE) 
          {
            std::string errmsg(sqlite3_errmsg(dbPtr));
            sqlite3_finalize(stmt);
            throw errmsg;
          }

        if(rc != SQLITE_DONE)   // When there is nothing to read
          {
            int id = sqlite3_column_int(stmt, 0);
            std::string propstr = (reinterpret_cast<const char*>(sqlite3_column_text(stmt, 1)));
            
            int field_occurrences = 0;
            std::string::size_type start = 0;
            
            while ((start = propstr.find(ftf.str(), start)) != std::string::npos) 
              {
                ++field_occurrences;
                start += ftf.str().length(); 
              }
            
            
            std::cout << "ID   :  " << sqlite3_column_int(stmt, 0) << std::endl;
            std::cout << "PROPS:  " << sqlite3_column_text(stmt, 1) << std::endl;
            std::cout << "MULT :  " << field_occurrences << std::endl;
            
            std::map<size_t, std::set<size_t> >::iterator vit = fm.find(field_occurrences);
            if (vit != fm.end())
              vit->second.insert(id);
            else
              {
                std::set<size_t> mult;
                mult.insert(id);
                fm[field_occurrences] = mult;
              }
          }
      }
    while(rc == SQLITE_ROW );   // When we have ROW to read

    if(rc == SQLITE_DONE) 
      {
        std::cout << "In done" << std::endl;
        sqlite3_finalize(stmt);
        return true;
      }
    else
      return false;
  }

  bool findField2(const std::string& f, std::map<size_t, std::vector<size_t> >& fm)
  {
    std::stringstream likeQuery;
    likeQuery << "SELECT PROPS FROM DIAGRAMS WHERE PROPS LIKE "
              << "\'%["
              << f 
              << "]%\'";
    
    
    char *zErrMsg = 0;
    int rc;
    // char *sql;
    const char* data = "Callback function called";
    
    std::map<size_t, std::vector<size_t> > rr;
    
    /* Execute SQL statement */
    rc = sqlite3_exec(dbPtr, likeQuery.str().c_str(), findSubstrings, reinterpret_cast<void*>(&rr), &zErrMsg);
    if( rc != SQLITE_OK ){
      fprintf(stderr, "SQL error: %s\n", zErrMsg);
      sqlite3_free(zErrMsg);
    }else{
      fprintf(stdout, "Operation done successfully\n");
    }

    return true;
  }
  
};




struct DBFactory
{
  static std::map< std::string, size_t > directory;
  static std::vector< QgrafSQL > vdb;

  static bool getDB(const std::string& dbname, QgrafSQL& db, size_t& num)
  {
    std::map< std::string, size_t >::iterator i = directory.find(dbname);
    if (i != directory.end())
      {
        std::cout << " Found" <<std::endl;
        num = i->second + 1;
        db  = vdb[i->second];
        return true;
      }
    else
      {
        std::cout << " Not found" <<std::endl;

        if(boost::filesystem::exists( dbname))
          {
            vdb.push_back(QgrafSQL(dbname,false));
            directory[dbname] = vdb.size() - 1;
            num = vdb.size();
            db  = vdb.back();
            return true;
          }
        else
          {
            return false;
          }
      }
  }

  static QgrafSQL& getDBbyNum(size_t dbnum)
  {
    std::cout << " By NUM length: " << vdb.size() << std::endl; 
    return vdb[dbnum - 1];
  }

  static size_t size()
  {
    return vdb.size();
  }

};

std::map< std::string, size_t > DBFactory::directory;
std::vector< QgrafSQL > DBFactory::vdb;


void LoadQGRAF(const unsigned char * str,const int len) 
{

  std::string fname;
  fname.append(reinterpret_cast<const char*>(str));

  boost::filesystem::path yaml_path( fname );
  boost::filesystem::path sql_path( yaml_path );

  yaml_path.replace_extension( "yaml" );
  sql_path.replace_extension( "sqlite3" );
  
  mprintln(std::string("fname ") + sql_path.string());
  
  
  if(!boost::filesystem::exists(sql_path))
    {
      // Check if YAML file exists
      if(boost::filesystem::exists( yaml_path.string() ))
        {
          Timer tmr;
          
          mprint("<-1-> Loading diagrams from file ");
          mprint(yaml_path.string());
          mprint(" ... ");
          
          std::vector<YAML::Node> diagrams = YAML::LoadAllFromFile(yaml_path.string());
          
          std::stringstream s1;
          s1 << "loaded " << diagrams .size() << " diagrams in " << tmr.get_s() << " s\n";
          mprint(s1.str());
          
          
          // Now dump all diagrams to SQL table
          
          tmr.reset();
          mprint("<-2-> Preparing SQL db ");
          mprint(sql_path.string());
          mprint(" ... ");
          
          // Open new DB
          QgrafSQL qsql(sql_path.string(), true);
          
          // Create table
          qsql.create();
          
          qsql.insert(diagrams);

          // for(std::vector<YAML::Node>::const_iterator dit = diagrams.begin(); 
          //     dit != diagrams.end(); ++dit)
          //   {
          //     DiagramRecord diagram_record(*dit);
          //     qsql.insert(diagram_record);
          //   }
          
          
          
          std::stringstream s2;
          s2 << "prepared in " << tmr.get_s() << " s\n";
          mprint(s2.str());
          
          // And return number of diagram generated by QGRAF
          MLPutInteger(stdlink, diagrams.size());
        }
      else
        {
          message("LoadQGRAF::noinput", yaml_path.string());
          
          // And return number of diagram generated by QGRAF
          MLPutSymbol(stdlink, "Null");
        }
      
    }
  else
    {
      message("LoadQGRAF::dbexists", sql_path.string());
      
      // And return number of diagram generated by QGRAF
      MLPutSymbol(stdlink, "Null");
    }
}


void Echo(const unsigned char * str,const int len)
{
  std::string s;
  s.append(reinterpret_cast<const char*>(str));
    
  MLPutString(stdlink, const_cast<char*>(s.c_str()));
}

void TestCMD(const unsigned char * str,const int len)
{

  std::stringstream prntmsg;
  prntmsg << "WriteString[\"stdout\", \"First part of the result:\"]";
  std::string temp = prntmsg.str();
  
  MLEvaluateString(stdlink, const_cast<char*>(temp.c_str()));



  std::stringstream prntmsg2;
  prntmsg2 << "WriteString[\"stdout\", \"... Second part of the result:\n\"]";
  temp = prntmsg2.str();
  
  MLEvaluateString(stdlink, const_cast<char*>(temp.c_str()));
  
  
  MLPutInteger(stdlink, 0);
}

// File name should be str.sqlite3
void LoadDB(const unsigned char * str,const int len)
{
  std::string fname;
  fname.append(reinterpret_cast<const char*>(str));
  boost::filesystem::path sql_path( fname );
  sql_path.replace_extension( "sqlite3" );


  // QgrafSQL qsql(sql_path.string(), false);
  
  size_t dbnum = 0;
  QgrafSQL qsql;
  if(DBFactory::getDB(sql_path.string(), qsql, dbnum))
    {
      
      if(qsql.ok())
        {
          std::stringstream s1;
          s1 << "<-1-> Open SQL db " << sql_path.string() << " into slot number " << dbnum;
          mprintln(s1.str());
          MLPutInteger(stdlink, dbnum);
        }
      else
        {
          std::stringstream s1;
          s1 << "ERROR opening  SQL db " << sql_path.string();
          mprintln(s1.str());
          MLPutSymbol(stdlink, "Null");
        }
    }
  else
    {
      message("LoadDB::noinput", sql_path.string());
      MLPutSymbol(stdlink,"Null");
    }
  
}

void GetDia(int n, int dbnum)
{
  
  if(dbnum <= DBFactory::size())
    {
      
      QgrafSQL qsql = DBFactory::getDBbyNum(dbnum);
      
      // qsql.find(n);
      DiagramRecord dr;
      if(qsql.select(n, dr))
        {
          MLPutFunction(stdlink, "Dia", 5);
          // 
          MLPutFunction(stdlink, "Rule", 2);
          MLPutSymbol  (stdlink, "id");
          MLPutInteger (stdlink, dr.id);
          // 
          MLPutFunction(stdlink, "Rule", 2);
          MLPutSymbol  (stdlink, "loops");
          MLPutInteger (stdlink, dr.loops);
          // 
          MLPutFunction(stdlink, "Rule", 2);
          MLPutSymbol  (stdlink, "factor");
          MLPutInteger (stdlink, dr.factor);
          // 
          MLPutFunction(stdlink, "List", dr.props.size() + dr.legs.size());
          for(std::vector<Prop>::const_iterator pi = dr.props.begin(); pi != dr.props.end(); ++pi)
            {
              
              MLPutFunction(stdlink, "List", 2);
              MLPutFunction(stdlink, "Rule", 2);
              MLPutInteger (stdlink, pi->u);
              MLPutInteger (stdlink, pi->v);
              MLPutFunction(stdlink, pi->type.c_str(), 1);
              MLPutSymbol  (stdlink, pi->field.c_str());
              
            }
          for(std::vector<Leg>::const_iterator li = dr.legs.begin(); li != dr.legs.end(); ++li)
            {
              
              MLPutFunction(stdlink, "List", 2);
              MLPutFunction(stdlink, "Rule", 2);
              MLPutInteger (stdlink, li->u);
              MLPutInteger (stdlink, li->v);
              MLPutFunction(stdlink, li->type.c_str(), 1);
              MLPutSymbol  (stdlink, li->field.c_str());
              
            }

          // List of momentums
          MLPutFunction(stdlink, "List", dr.props.size());
          for(std::vector<Prop>::const_iterator pi = dr.props.begin(); pi != dr.props.end(); ++pi)
            {
              MLPutFunction(stdlink, "ToExpression", 1);
              MLPutString(stdlink, pi->mom.c_str());              
            }

 
        }
      else
        {
          std::cout << "Diagram not found in DB" << std::endl;
          MLPutSymbol(stdlink, "Null");
        }
    }
  else
    {
      std::stringstream s;
      s << "ERROR: SQL DB in slot " << dbnum << " not loaded";
      mprintln(s.str());
      MLPutSymbol(stdlink, "Null");
    }
}




void GetDiaGraph(int n, int dbnum)
{

  if(dbnum <= DBFactory::size())
    {
      
      QgrafSQL qsql = DBFactory::getDBbyNum(dbnum);
      
      // qsql.find(n);
      DiagramRecord dr;
      if(qsql.select(n, dr))
        {
          MLPutFunction(stdlink, "List", 3);
          // 
          MLPutFunction(stdlink, "Rule", 2);
          MLPutSymbol  (stdlink, "id");
          MLPutInteger (stdlink, dr.id);
          // 
          MLPutFunction(stdlink, "Rule", 2);
          MLPutSymbol  (stdlink, "loops");
          MLPutInteger (stdlink, dr.loops);
          // 
          MLPutFunction(stdlink, "Rule", 2);
          MLPutSymbol  (stdlink, "factor");
          MLPutInteger (stdlink, dr.factor);
        }
      else
        {
          std::cout << "Diagram not found in DB" << std::endl;
          MLPutSymbol(stdlink, "Null");
        }
    }
  else
    {
      std::stringstream s;
      s << "ERROR: SQL DB in slot " << dbnum << " not loaded";
      mprintln(s.str());
      MLPutSymbol(stdlink, "Null");
    }
}

void WithField(const unsigned char * str,const int len, int dbnum)
{
  if(dbnum <= DBFactory::size())
    {
      
      QgrafSQL qsql = DBFactory::getDBbyNum(dbnum);
      
      std::string field;
      field.append(reinterpret_cast<const char*>(str));

      mprint("Start field ");
      mprintln(field);
      
      std::map<size_t, std::set<size_t> > fm;
      if(qsql.findField(field, fm))
        {
          MLPutFunction(stdlink, "List", fm.size());
          // 
          for(std::map<size_t, std::set<size_t> >::const_iterator mulit = fm.begin(); mulit != fm.end(); ++ mulit)
            {
              MLPutFunction(stdlink, "Field", 3);
              MLPutSymbol  (stdlink, field.c_str());
              MLPutInteger (stdlink, mulit->first);
              // 
              MLPutFunction(stdlink, "List", mulit->second.size());
              for(std::set<size_t>::const_iterator idit = mulit->second.begin(); idit != mulit->second.end(); ++idit)
                MLPutInteger (stdlink, *idit);
            }
        }
      else
        {
          std::cout << "No diagrams fith field " << field << " found" << std::endl;
          MLPutSymbol(stdlink, "Null");
        }
    }
  else
    {
      std::stringstream s;
      s << "ERROR: SQL DB in slot " << dbnum << " not loaded";
      mprintln(s.str());
      MLPutSymbol(stdlink, "Null");
    }

}


void WithFieldType(const unsigned char * str,const int len, int dbnum)
{
  if(dbnum <= DBFactory::size())
    {
      
      QgrafSQL qsql = DBFactory::getDBbyNum(dbnum);
      
      std::string fieldtype;
      fieldtype.append(reinterpret_cast<const char*>(str),len);
      
      std::map<size_t, std::set<size_t> > fm;
      if(qsql.findFieldType(fieldtype, fm))
        {
          MLPutFunction(stdlink, "List", fm.size());
          // 
          for(std::map<size_t, std::set<size_t> >::const_iterator mulit = fm.begin(); mulit != fm.end(); ++ mulit)
            {
              MLPutFunction(stdlink, "Field", 3);
              MLPutSymbol  (stdlink, fieldtype.c_str());
              MLPutInteger (stdlink, mulit->first);
              // 
              MLPutFunction(stdlink, "List", mulit->second.size());
              for(std::set<size_t>::const_iterator idit = mulit->second.begin(); idit != mulit->second.end(); ++idit)
                MLPutInteger (stdlink, *idit);
            }
        }
      else
        {
          std::cout << "No diagrams fith field " << fieldtype << " found" << std::endl;
          MLPutSymbol(stdlink, "Null");
        }
    }
  else
    {
      std::stringstream s;
      s << "ERROR: SQL DB in slot " << dbnum << " not loaded";
      mprintln(s.str());
      MLPutSymbol(stdlink, "Null");
    }

}


void FermionFlow(int n, int dbnum)
{

  if(dbnum <= DBFactory::size())
    {
      
      QgrafSQL qsql = DBFactory::getDBbyNum(dbnum);
      
      // qsql.find(n);
      DiagramRecord dr;
      if(qsql.select(n, dr))
        {

          FermionGraph fg(dr);
          
          MLPutFunction(stdlink, "List", 2);
          MLPutInteger (stdlink, fg.open());
          MLPutInteger (stdlink, fg.closed());
        }
      else
        {
          std::cout << "Diagram not found in DB" << std::endl;
          MLPutSymbol(stdlink, "Null");
        }
    }
  else
    {
      std::stringstream s;
      s << "ERROR: SQL DB in slot " << dbnum << " not loaded";
      mprintln(s.str());
      MLPutSymbol(stdlink, "Null");
    }
        
}


void Tadpoles(int n, int dbnum)
{

  if(dbnum <= DBFactory::size())
    {
      
      QgrafSQL qsql = DBFactory::getDBbyNum(dbnum);
      
      // qsql.find(n);
      DiagramRecord dr;
      if(qsql.select(n, dr))
        {
          
          TadpoleGraph tg(dr);
          
          MLPutFunction(stdlink, "Subgraphs", tg.getSubgraphs().size() + tg.tad1l().size());
          
          // One-loop tadpoles
          for(std::vector<size_t>::const_iterator ei = tg.tad1l().begin(); ei != tg.tad1l().end(); ++ei)
            {
              MLPutFunction(stdlink, "List", 2);
              MLPutFunction(stdlink, "Tad", 1);
              MLPutFunction(stdlink, "List", 2);
              MLPutFunction(stdlink, "Rule", 2);
              
              MLPutInteger (stdlink, dr.props[*ei - 1].u);
              MLPutInteger (stdlink, dr.props[*ei - 1].v);
              MLPutFunction(stdlink, dr.props[*ei - 1].type.c_str(), 1);
              MLPutSymbol  (stdlink, dr.props[*ei - 1].field.c_str());
              
              MLPutInteger (stdlink, 1);
            }
          
          // Other graphs

          size_t graph_num = 0;
          for(TadpoleGraph::SubgraphIds::const_iterator si = tg.getSubgraphs().begin(); si != tg.getSubgraphs().end(); ++si)
            {
              MLPutFunction(stdlink, "List", 2);
              if(si->first.size() == 1) // Diagram with single edge is a sbridge
                MLPutFunction(stdlink, "Sbridge", 1);
              else if(si->second) // Not a Tadpole
                MLPutFunction(stdlink, "Dia", si->first.size());
              else
                MLPutFunction(stdlink, "Tad", si->first.size());
              
              
              for (std::vector<int>::const_iterator ei = si->first.begin(); ei != si->first.end(); ++ei)
                {

                  MLPutFunction(stdlink, "List", 2);
                  MLPutFunction(stdlink, "Rule", 2);
                  if (*ei > 0)  // Propagator
                    {
                      MLPutInteger (stdlink, dr.props[*ei - 1].u);
                      MLPutInteger (stdlink, dr.props[*ei - 1].v);
                      MLPutFunction(stdlink, dr.props[*ei - 1].type.c_str(), 1);
                      MLPutSymbol  (stdlink, dr.props[*ei - 1].field.c_str());
                    }
                  else          // Leg
                    {
                      MLPutInteger (stdlink, dr.legs[-(*ei) - 1].u);
                      MLPutInteger (stdlink, dr.legs[-(*ei) - 1].v);
                      MLPutFunction(stdlink, dr.legs[-(*ei) - 1].type.c_str(), 1);
                      MLPutSymbol  (stdlink, dr.legs[-(*ei) - 1].field.c_str());
                    }

                  std::cout << "      ee id = " << *ei << std::endl;

                }
              // L=E-V+1  
              size_t sub_loops = tg.getSubgraphs()[graph_num].first.size() - tg.verts(graph_num).size() + 1;
              MLPutInteger (stdlink, sub_loops);
              graph_num++;
            }


          // MLPutFunction(stdlink, "List", 2);
          // MLPutInteger (stdlink, fg.open());
          // MLPutInteger (stdlink, tg.numTadpoles());
          
          // MLPutSymbol(stdlink, "Null");
        }
      else
        {
          std::cout << "Diagram not found in DB" << std::endl;
          MLPutSymbol(stdlink, "Null");
        }
    }
  else
    {
      std::stringstream s;
      s << "ERROR: SQL DB in slot " << dbnum << " not loaded";
      mprintln(s.str());
      MLPutSymbol(stdlink, "Null");
    }
  
}



void NoSigma(int n, int dbnum)
{

  if(dbnum <= DBFactory::size())
    {
      
      QgrafSQL qsql = DBFactory::getDBbyNum(dbnum);
      
      // qsql.find(n);
      DiagramRecord dr;
      if(qsql.select(n, dr))
        {
          
          SigmaFinder sg(dr);

          std::cout << "Open list size " << sg.base().size() << std::endl;
          MLPutFunction(stdlink, "List", sg.base().size());
          // MLPutInteger (stdlink, fg.open());

          // Only mapped unique edges we mapped on
          for(std::set<size_t>::const_iterator pi = sg.base().begin(); pi != sg.base().end(); ++pi)
            {
              MLPutFunction(stdlink, "Rule", 2);
              MLPutInteger (stdlink, dr.props[*pi].u);
              MLPutInteger (stdlink, dr.props[*pi].v);
              
            }
          
          // MLPutSymbol(stdlink, "Null");
        }
      else
        {
          std::cout << "Diagram not found in DB" << std::endl;
          MLPutSymbol(stdlink, "Null");
        }
    }
  else
    {
      std::stringstream s;
      s << "ERROR: SQL DB in slot " << dbnum << " not loaded";
      mprintln(s.str());
      MLPutSymbol(stdlink, "Null");
    }
  
}


int main(int argc, char* argv[]) 
{
  return MLMain(argc, argv);
}
