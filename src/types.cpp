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
