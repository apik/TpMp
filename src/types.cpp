#include "types.hpp"

#include <iostream>
void Prop::fromStr(size_t nid, const std::string& s)
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

void Leg::fromStr(size_t nid, const std::string& s)
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

DiagramRecord::DiagramRecord(YAML::Node n)
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

  // Check that all external legs input in qgraf.dat as incoming,
  // because we parse only incoming legs. It is needed for all
  // momentums have same direction and symbols w/o "-" sign
  if(n.begin()->second["extern"].as<int>() > n.begin()->second["legs"].size())
    throw;
  
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

  std::cout << n.begin()->second["verts"] << std::endl;

  for (YAML::const_iterator it = n.begin()->second["verts"].begin(); it != n.begin()->second["verts"].end(); ++it) 
    {
      Vert v;
      v.id     = it->first.as<int>();
      v.type   = it->second["struct"].as<std::string>();

      std::cout << "vlen: " << it->second["fields"].size() << std::endl;

      std::vector<std::string> ftypes;
      for (YAML::const_iterator fit = it->second["fields"].begin(); 
           fit != it->second["fields"].end(); ++fit) 
        {
          ftypes.push_back(fit->as<std::string>());
          std::cout << fit->as<std::string>() << std::endl;
        }

      std::vector<int> fids;
      
      for (YAML::const_iterator fit = it->second["props"].begin(); 
           fit != it->second["props"].end(); ++fit) 
        {
          int rawid = fit->as<int>();
          // Propagator
          if(rawid > 0)
            {
              v.ids.push_back(rawid);
              v.internal.push_back(true);
            }
          // External leg
          else
            {
              v.ids.push_back((1-rawid)/2); // we decode id=-2j+1
              v.internal.push_back(false);
            }
        }
      
      verts.push_back(v);
    }

}

void DiagramRecord::propStr(const std::string& s)
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

void DiagramRecord::legStr(const std::string& s)
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
