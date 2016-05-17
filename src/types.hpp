#ifndef __TYPES_HPP__
#define __TYPES_HPP__
#include <string>
#include <sstream>
#include <vector>
#include <yaml-cpp/yaml.h>

struct Prop
{
  int id;
  int u;
  int v;
  std::string field;
  std::string type;
  std::string mom;
  
  // Serialize to string <TYPE>[<FIELD>](<U>,<V>,<MOM>)
  inline std::string toStr() const
  {
    std::stringstream s;
    s << type << "[" << field << "](" << u << "," << v << "," << mom << ")";
    return s.str();
  }

  void fromStr(size_t, const std::string&);

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
  inline std::string toStr() const
  {
    std::stringstream s;
    s << type << "[" << field << "](" << u << "," << v << "," << mom << ")";
    return s.str();
  }

  void fromStr(size_t, const std::string&);

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
  
  DiagramRecord(YAML::Node);

  inline std::string propStr() const
  {
    std::stringstream s;
    for(std::vector<Prop>::const_iterator pi = props.begin(); pi != props.end(); ++pi)
        s << pi->toStr() << ";";
    return s.str();
  }

  void propStr(const std::string&);

  inline std::string legStr() const
  {
    std::stringstream s;
    for(std::vector<Leg>::const_iterator li = legs.begin(); li != legs.end(); ++li)
      s << li->toStr() << ";";
    return s.str();
  }

  void legStr(const std::string&);

};
#endif  // __TYPES_HPP__

