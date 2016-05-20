#ifndef __SIGMA_HPP__
#define __SIGMA_HPP__

#include "types.hpp"
#include <boost/graph/biconnected_components.hpp>
#include <boost/graph/filtered_graph.hpp>
#include <boost/fusion/iterator/next.hpp>

typedef boost::adjacency_list<multisetS, vecS, undirectedS, 
                              property<vertex_name_t, int>,
                              property < edge_index_t, size_t>
                              > SigmaGraph;


typedef boost::graph_traits<SigmaGraph>::edge_descriptor ED;

struct edge_to_remove 
{
  edge_to_remove() { }
  edge_to_remove(const ED& e1, const ED& e2) : m_e1(e1), m_e2(e2) { }
  
  template <typename Edge>
  bool operator()(const Edge& e) const {
    return e != m_e1 && e != m_e2;
  }

  ED m_e1;
  ED m_e2;
};


class SigmaFinder
{
  SigmaGraph g;
  
  
  std::vector<int> vnames;
  
public:
  SigmaFinder(const DiagramRecord& dr)
  {
  // property_map<Graph_t, edge_index_t>::type eim = get(edge_index, g);
  // property_map<Graph_t, edge_component_t>::type ecm = get(edge_component, g);

    
  // We find self-energy insertions only in internal lines
  
    for(std::vector<Prop>::const_iterator pi = dr.props.begin(); pi != dr.props.end(); ++pi)
      {
        VD u,v;
        std::vector<int>::iterator vit = std::find(vnames.begin(), vnames.end(), pi->u);
      
        // One-loop tadpoles not added to graph
        // They can not contribute to edges with SE insertion
        if(pi->u != pi->v)
          {
            if(vit  != vnames.end())
              {
                u = (vit - vnames.begin());
              
                std::cout << "u From vec " << u <<std::endl;
              }
            else
              {
                u = add_vertex(Graph_t::vertex_property_type(pi->u), g);
              
                std::cout << "u To   vec " << u <<std::endl;
                vnames.push_back(pi->u);
                assert(u == vnames.size() - 1);
              }
          
            vit = std::find(vnames.begin(), vnames.end(), pi->v);
            if( vit != vnames.end())
              {
                v = (vit - vnames.begin());
                std::cout << "v From vec " << v <<std::endl;
              }
            else
              {
                v = add_vertex(Graph_t::vertex_property_type(pi->v), g);
                std::cout << "v To   vec " << v <<std::endl;
                vnames.push_back(pi->v);
                assert(v == vnames.size() - 1);
              }

            Edge ee = add_edge (u, v, pi->id, g).first; 
        
            // Set to negative to mark parallel edges not included in map
            // ecm[ee] = -1;

            std::cout << "Edge " << pi->type << " added" << std::endl;
            // }
          }
      }


graph_traits < SigmaGraph >::edge_iterator ei1, ei2, ei_begin, ei_end;
    // graph_traits < SigmaGraph >::edge_iterator ei2, ei2_end;
tie(ei_begin, ei_end) = edges(g);
    for (ei1 = ei_begin; ei1 != ei_end; ++ei1)
      for (ei2 = ei_begin; ei2 != ei1; ++ei2)
      {
std::cout << "Removing " << *ei1 << " and " << *ei2 << std::endl;

if(degree(source(*ei1,g),g) > 1 && degree(target(*ei1,g),g) > 1 &&
degree(source(*ei2,g),g) > 1 && degree(target(*ei2,g),g) > 1)
  {
edge_to_remove filter(*ei1,*ei2);
filtered_graph<SigmaGraph, edge_to_remove> fg(g, filter);

std::vector<int> component(num_vertices(fg));
size_t con_comp = connected_components(fg, &component[0]);
std::cout << "Coooooon " << con_comp << std::endl;
}

      }        
  }
};


#endif  // __SIGMA_HPP__
