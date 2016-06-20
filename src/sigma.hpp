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

  // pair of equal edges
  // Complicted mappings are composed from pairs of elementary mapping pairs
  std::map<size_t,size_t> eq_edges;

  // TRUE if edge has same direction with edge it mapped on
  std::map<size_t,bool>   same_dir;

  // Vertex map for shrinked edges mapping
public:  
  std::map<size_t,size_t> v_shrink_to;
  
  // Graph containing only edges with different momenta
  std::set<size_t> no_sigma_graph;
public:
  SigmaFinder(const DiagramRecord& dr)
  {
    
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
          }
      }

    // When edges added and all vertices initialized
    // prepare map of vertices connected with legs

    std::vector<VD> external_v;
    for(std::vector<Leg>::const_iterator li = dr.legs.begin(); li != dr.legs.end(); ++li)
      {

        std::vector<int>::iterator vit = std::find(vnames.begin(), vnames.end(), li->v);
        if(vit  == vnames.end()) 
          throw;
        else
          external_v.push_back(vit - vnames.begin());
      }
    

    // Now remove all possible pairs of edges
    // And check that all external vertices belong to the
    // same connected component
    graph_traits < SigmaGraph >::edge_iterator ei1, ei2, ei_begin, ei_end;
    // graph_traits < SigmaGraph >::edge_iterator ei2, ei2_end;
    tie(ei_begin, ei_end) = edges(g);
    for (ei1 = ei_begin; ei1 != ei_end; ++ei1)
      for (ei2 = ei_begin; ei2 != ei1; ++ei2)
        {
          std::cout << "Removing " << *ei1 << " and " << *ei2 << std::endl;

          // We remove only edges with vertices degree more than 1
          // and not parallel
          if(
             (degree(source(*ei1,g),g) > 1 && degree(target(*ei1,g),g) > 1) &&
             (degree(source(*ei2,g),g) > 1 && degree(target(*ei2,g),g) > 1) &&
             !(source(*ei1,g) == source(*ei2,g) && target(*ei1,g) == target(*ei2,g)) &&
             !(source(*ei1,g) == target(*ei2,g) && target(*ei1,g) == source(*ei2,g))
             )
            {
              edge_to_remove filter(*ei1,*ei2);
              filtered_graph<SigmaGraph, edge_to_remove> fg(g, filter);

              std::vector<int> component(num_vertices(fg));
              size_t con_comp = connected_components(fg, &component[0]);
              std::cout << "Coooooon " << con_comp << std::endl;

              if(con_comp == 2)
                {
                  
                  size_t    component_external_legs;
                  bool legs_in_single_component = true;
                  for(std::vector<VD>::const_iterator evit = external_v.begin(); evit != external_v.end(); ++evit)
                    {
                      if(evit == external_v.begin())
                        component_external_legs = component[*evit];
                      else if(component_external_legs != component[*evit])
                        legs_in_single_component = false;
                    }
                  // Proceed only if all legs are in the same
                  // connected component

                  
                  if(legs_in_single_component)
                    {

                      std::cout << "Legs in single component!!!" << std::endl; 
                      // we eliminate edges with larger idx
                      // with smaller idx edges
                      size_t lhs = std::max(get(edge_index, g, *ei1), get(edge_index, g, *ei2));
                      size_t rhs = std::min(get(edge_index, g, *ei1), get(edge_index, g, *ei2));
                      eq_edges[lhs] = rhs;
                      
                      // LHS is edge to be shrinked and we fill vertex shrink map
                      // We keep vertex with larger number to highlight removed number!!!
                      v_shrink_to[std::min(dr.props[lhs].u,dr.props[lhs].v)] = std::max(dr.props[lhs].u,dr.props[lhs].v);
                      std::cout << "Now vertex " << std::min(dr.props[lhs].u,dr.props[lhs].v) << " become " << std::max(dr.props[lhs].u,dr.props[lhs].v) << std::endl;

                      if(component[source(*ei1,g)] == component[source(*ei2,g)])
                        same_dir[lhs] = false; // oposit
                      else if(component[source(*ei1,g)] == component[target(*ei2,g)])
                        same_dir[lhs] = true;  // same
                      else throw;
                    }
                }
            }

        }

    // eq_edges can contain recursive edges substitution
    // like 3->2, 2->1 ,4->1 which is equal to (3,2,4)->1

    for(std::map<size_t,size_t>::iterator eqit = eq_edges.begin(); eqit != eq_edges.end(); ++eqit)
      {
        size_t rhs = eqit->second;
        std::map<size_t,size_t>::iterator it;
        while((it = eq_edges.find(rhs)) != eq_edges.end())
          {
            rhs = it->second;
            eqit->second = rhs;
          }

        no_sigma_graph.insert(rhs);
      }
    
  }



  const std::map<size_t,size_t>&  emap() const
  {
    return eq_edges;
  }
  
  
  const std::map<size_t,bool>&    codir() const
  {
    return same_dir;
  }

  const std::set<size_t>& base() const
  {
    return no_sigma_graph;
  }

  bool is_base(size_t e) const
  {
    return no_sigma_graph.count(e) == 1;
  }

  
};


#endif  // __SIGMA_HPP__
