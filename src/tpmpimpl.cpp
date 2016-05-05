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




// const QgrafSQL & get_QgrafSQL(const std::string& dbname)
// {
//   static std::map< std::string, size_t > directory;
//   static std::vector< QgrafSQL > vdb;
//   std::map< std::string, size_t >::iterator i = directory.find(dbname);
//   if (i != directory.end())
//     return vdb[i->second];
//   else
//     return directory.insert(std::make_pair(std::make_pair(mi,mu2), ZZ<MS>(mi,mu2))).first->second;
// }




// class customer
// {
//     ...
//     public:
//   int callback(int argc, char **argv, char **azColName);
// };

// static int c_callback(void *param, int argc, char **argv, char **azColName)
// {
//   customer* cust = reinterpret_cast<customer*>(param);
//   return cust->callback(argc, argv, azColName);
// }

// char* customer::getCustomer(int id)
// {
//     ...
//   rc = sqlite3_exec(db, sql, c_callback, this, &errMsg);
//     ...
//     }

// int customer::callback(int argc, char **argv, char **azColName)
// {
//     ...
// }



// std::string sql_insert(int id, int loops)
// {
//   std::stringstream insertQuery;
//   insertQuery << "INSERT INTO DIAGRAMS (ID,LOOPS,PROPS,LEGS,VERTS)"
//     " VALUES (" 
//               << id << ", " 
//               << loops << ", " 
//               << quoted("prpr") << ", " 
//               << quoted("leggs") << ", " 
//               << quoted("vertsxx") << ");";
  
//   return insertQuery.str();
// }

// const char* SQL = "CREATE TABLE IF NOT EXISTS foo(a,b,c); INSERT INTO FOO VALUES(1,2,3); INSERT INTO FOO SELECT * FROM FOO;";


class Timer
{
  std::chrono::time_point<std::chrono::system_clock> ts;
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
  int get_s()
  {
    return std::chrono::duration_cast<std::chrono::seconds>(std::chrono::high_resolution_clock::now() - ts).count();
  }
};

void mprint(const std::string& s)
{
  std::stringstream prntmsg;
  prntmsg << "Print[\"" << s << "\"]";
  std::string temp = prntmsg.str();

  MLEvaluateString(stdlink, const_cast<char*>(temp.c_str()));
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

  QgrafSQL(const std::string& dbname, bool overwritedb)
  {
    
    int flags = 0;
    if(overwritedb)
      flags = SQLITE_OPEN_READWRITE | SQLITE_OPEN_CREATE;
    else
      flags = SQLITE_OPEN_READWRITE;
    
    if( sqlite3_open_v2(dbname.c_str(), &dbPtr, flags, NULL) )
      {
        mprint(sqlite3_errmsg(dbPtr));
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
            mprint(err);
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
      mprint("Didn't Insert Item!");
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
    if( rc != SQLITE_OK ){
      fprintf(stderr, "SQL error: %s\n", zErrMsg);
      sqlite3_free(zErrMsg);
    }else{
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
  
};




struct DBFactory
{
  static std::map< std::string, size_t > directory;
  static std::vector< QgrafSQL > vdb;

  static QgrafSQL& getDB(const std::string& dbname, size_t& num)
  {
    std::map< std::string, size_t >::iterator i = directory.find(dbname);
    if (i != directory.end())
      {
        std::cout << " Found" <<std::endl;
        num = i->second + 1;
        return vdb[i->second];
      }
    else
      {
        std::cout << " Not found" <<std::endl;
        vdb.push_back(QgrafSQL(dbname,false));
        directory[dbname] = vdb.size() - 1;
        num = vdb.size();
        return vdb.back();
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

  sql_path.replace_extension( "sqlite3" );
  
  mprint(std::string("fname ") + sql_path.string());
  
  
  if(!boost::filesystem::exists(sql_path))
    {
      
      Timer tmr;
      
      std::vector<YAML::Node> diagrams = YAML::LoadAllFromFile(fname);
      
      std::stringstream s1;
      s1 << "<-1-> Loaded " << diagrams .size() << " diagrams from file " << fname << " in " << tmr.get_ms() << " ms";
      mprint(s1.str());
      
      
      // Now dump all diagrams to SQL table
      
      tmr.reset();
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
      s2 << "<-2-> Prepared SQL db " << sql_path.string() << " in " << tmr.get_ms() << " ms";
      mprint(s2.str());
      
      // And return number of diagram generated by QGRAF
      MLPutInteger(stdlink, diagrams.size());
      
    }
  else
    {
      std::stringstream s2;
      s2 << "<-2-> SQL db " << sql_path.string() << " exists";
      mprint(s2.str());
      
      // And return number of diagram generated by QGRAF
      MLPutInteger(stdlink, 0);

    }
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
  QgrafSQL qsql = DBFactory::getDB(sql_path.string(), dbnum);

  if(qsql.ok())
    {
      std::stringstream s1;
      s1 << "<-1-> Open SQL db " << sql_path.string() << " into slot number " << dbnum;
      mprint(s1.str());
    }
  else
    {
      std::stringstream s1;
      s1 << "ERROR opening  SQL db " << sql_path.string();
      mprint(s1.str());
    }
  
  MLPutInteger(stdlink, dbnum);
  
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
      mprint(s.str());
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
      mprint(s.str());
      MLPutSymbol(stdlink, "Null");
    }
}



  

int main(int argc, char* argv[]) 
{
  return MLMain(argc, argv);
}
