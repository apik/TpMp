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

struct Prop
{
  int id;
  int u;
  int v;
  std::string field;
  std::string type;
  std::string mom;
  
  // Serialize to string <TYPE>[<FIELD>](<U>,<V>,<MOM>)
  std::string toStr() const
  {
    std::stringstream s;
    s << type << "[" << field << "](" << u << "," << v << "," << mom << ")";
    return s.str();
  }

  void fromStr(size_t nid, std::string s)
  {
    id = nid;
    size_t start = 0;
    size_t end   = s.find('['); 
    std::cout << "TYPE = " << s.substr(start,end) << std::endl;
    type = s.substr(start,end - start);

    start = end + 1;
    end = s.find(']', start);
    std::cout << "(s,e) = " << start << "," << end << " FIELD = " << s.substr(start,end - start) << std::endl;
    field = s.substr(start,end - start);

    start = end + 2;            // ](
    end = s.find(',', start);
    std::cout << "u = " << s.substr(start,end - start) << std::endl;
    u = atoi(s.substr(start,end - start).c_str());

    start = end + 1;            // ](
    end = s.find(',', start);
    std::cout << "v = " << s.substr(start,end - start) << std::endl;
    v = atoi(s.substr(start,end - start).c_str());

    start = end + 1;            // ](
    end = s.find(')', start);
    std::cout << "mom = " << s.substr(start,end - start) << std::endl;
    mom = s.substr(start,end - start);

  }

};

struct Leg
{
  int id;
  int u;
  int v;
  std::string field;
  std::string type;
  std::string mom;
  
  // Serialize to string <TYPE>[<FIELD>](<U>,<V>,<MOM>)
  std::string toStr() const
  {
    std::stringstream s;
    s << type << "[" << field << "](" << u << "," << v << "," << mom << ")";
    return s.str();
  }

  void fromStr(size_t nid, std::string s)
  {
    id = nid;
    size_t start = 0;
    size_t end   = s.find('['); 
    std::cout << "TYPE = " << s.substr(start,end) << std::endl;
    type = s.substr(start,end - start);

    start = end + 1;
    end = s.find(']', start);
    std::cout << "(s,e) = " << start << "," << end << " FIELD = " << s.substr(start,end - start) << std::endl;
    field = s.substr(start,end - start);

    start = end + 2;            // ](
    end = s.find(',', start);
    std::cout << "u = " << s.substr(start,end - start) << std::endl;
    u = atoi(s.substr(start,end - start).c_str());

    start = end + 1;            // ](
    end = s.find(',', start);
    std::cout << "v = " << s.substr(start,end - start) << std::endl;
    v = atoi(s.substr(start,end - start).c_str());

    start = end + 1;            // ](
    end = s.find(')', start);
    std::cout << "mom = " << s.substr(start,end - start) << std::endl;
    mom = s.substr(start,end - start);

  }

};

struct Vert
{
  int id;
  int u;
  int v;
  std::string field;
  int type;
  std::string mom;
};


class DiagramRecord
{
  
public:
  int id;
  int loops;
  int factor;
  int sign;
  std::vector<Prop> props;
  std::vector<Leg>  legs;
  DiagramRecord()
  {
  }

  DiagramRecord(YAML::Node n)
  {
    id     = n.begin()->first.as<int>();
    loops  = n.begin()->second["loops"].as<int>();
    factor = n.begin()->second["factor"].as<int>();
    
    // sign   = n.second["sign"].as<int>();

    // Filling propagators array
    std::cout << n.begin()->second["props"] << std::endl;
    for (YAML::const_iterator it = n.begin()->second["props"].begin(); it != n.begin()->second["props"].end(); ++it) 
      {
        Prop p;
        p.id     = it->first.as<int>();
        p.u      = it->second["u"].as<int>();
        p.v      = it->second["v"].as<int>();
        p.field  = it->second["f"].as<std::string>();
        p.type   = it->second["t"].as<std::string>();
        p.mom    = it->second["m"].as<std::string>();

        props.push_back(p);
      }

    // Filling legs array
    std::cout << n.begin()->second["legs"] << std::endl;
    for (YAML::const_iterator it = n.begin()->second["legs"].begin(); it != n.begin()->second["legs"].end(); ++it) 
      {
        Leg l;
        l.id     = it->first.as<int>();
        l.u      = it->second["u"].as<int>();
        l.v      = it->second["v"].as<int>();
        l.field  = it->second["f"].as<std::string>();
        l.type   = it->second["t"].as<std::string>();
        l.mom    = it->second["m"].as<std::string>();

        legs.push_back(l);
      }

    std::cout << "We add " << props.size() << " props" << std::endl;
  }

  std::string propStr() const
  {
    std::stringstream s;
    for(std::vector<Prop>::const_iterator pi = props.begin(); pi != props.end(); ++pi)
        s << pi->toStr() << ";";
    return s.str();
  }

  void propStr(const std::string& s)
  {
    std::cout << "Reconstructing props from " << s << std::endl;

    size_t start = 0;
    size_t end = s.find(';');
    while (end != std::string::npos)
    {
        std::cout << s.substr(start, end - start) << std::endl;

        Prop p;
        p.fromStr(props.size() + 1, s.substr(start, end - start));
        props.push_back(p);
        
        start = end + 1;
        end = s.find(';', start);
    }

    std::cout << "REMINDER  " << s.substr(start, end) << std::endl;;        
  }

  std::string legStr() const
  {
    std::stringstream s;
    for(std::vector<Leg>::const_iterator li = legs.begin(); li != legs.end(); ++li)
      s << li->toStr() << ";";
    return s.str();
  }

  void legStr(const std::string& s)
  {
    std::cout << "Reconstructing props from " << s << std::endl;

    size_t start = 0;
    size_t end = s.find(';');
    while (end != std::string::npos)
    {
        std::cout << s.substr(start, end - start) << std::endl;

        Leg l;
        l.fromStr(legs.size() + 1, s.substr(start, end - start));
        legs.push_back(l);
        
        start = end + 1;
        end = s.find(';', start);
    }

    std::cout << "REMINDER  " << s.substr(start, end) << std::endl;;        
  }

};



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


  bool insert(const DiagramRecord& dia)
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
    std::stringstream likeQuery;
    likeQuery << "SELECT ID,PROPS FROM DIAGRAMS WHERE PROPS LIKE "
              << "\'%["
              << f 
              << "]%\'";
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
          
          for(std::vector<YAML::Node>::const_iterator dit = diagrams.begin(); 
              dit != diagrams.end(); ++dit)
            {
              DiagramRecord diagram_record(*dit);
              qsql.insert(diagram_record);
            }
          
          
          
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
          MLPutFunction(stdlink, "List", 5);
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
      
      std::map<size_t, std::set<size_t> > fm;
      if(qsql.findField(field, fm))
        {
          MLPutFunction(stdlink, "List", fm.size());
          // 
          for(std::map<size_t, std::set<size_t> >::const_iterator mulit = fm.begin(); mulit != fm.end(); ++ mulit)
            {
              MLPutFunction(stdlink, "Rule", 2);
              // 
              MLPutFunction(stdlink, "Field", 2);
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


int main(int argc, char* argv[]) 
{
  return MLMain(argc, argv);
}
