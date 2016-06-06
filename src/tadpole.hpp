#ifndef __TADPOLE_HPP__
#define __TADPOLE_HPP__

#include "types.hpp"
#include <boost/graph/biconnected_components.hpp>
#include <boost/graph/depth_first_search.hpp>



// class dummy_visitor:public default_dfs_visitor 
// {
//   // typedef typename property_traits < TimeMap >::value_type T;
// public:

//   dummy_visitor(int t)
//     :  m_time(t) 
//   {
//   }
  
//   template < typename Vertex, typename Graph >
//     void discover_vertex(Vertex u, const Graph & g) const
//   {
//     std::cout << "Discovered vertex " << u << std::endl;
//     // put(m_dtimemap, u, m_time++);
//   }

//   template < typename Vertex, typename Graph >
//     void finish_vertex(Vertex u, const Graph & g) const
//   {
//     // put(m_ftimemap, u, m_time++);
//   }
//   int & m_time;
// };



namespace boost
{
  struct edge_component_t
  {
    enum
      { num = 555 };
    typedef edge_property_tag kind;
  }
    edge_component;
}

typedef boost::adjacency_list<multisetS, vecS, undirectedS, 
                              property<vertex_name_t, int>,
                              property < edge_index_t, int, property<edge_component_t, int> >
                              > Graph_t;



typedef graph_traits<Graph_t>::edge_descriptor Edge;

// 
// This type of graph when created contains information
// about full set of tadpole subgraph i.e. connected through
// the singular bridge(zero momentum) ")-O" or snail type ")O".
// 
class TadpoleGraph
{
public:

  // Boolean flag "NotATadpole" set if negative edges present
  typedef std::vector<std::pair<std::vector<int>, bool> > SubgraphIds;
  
private:
  Graph_t g;
  
  
  size_t snails;
  std::vector<int> vnames;
  
  // List of one-loop tadpole edge numbers
  std::vector<size_t> self_edges;
  
  SubgraphIds subgraphs;

  // std::vector<size_t> count_loops;
  std::vector<std::set<int>> count_verts;
  
public:
  TadpoleGraph(const DiagramRecord& dr)
  {

    // First we add legs, connecting all legs on single vertex "0"

    property_map<Graph_t, edge_index_t>::type eim = get(edge_index, g);
    property_map<Graph_t, edge_component_t>::type ecm = get(edge_component, g);
    
    for(std::vector<Leg>::const_iterator li = dr.legs.begin(); li != dr.legs.end(); ++li)
      {

        // U - is negative, eternal
        // V - is positive, internal
        VD u,v;

        // We use single external vertex for all external legs
        if(vnames.size() == 0 )
          {
            u = add_vertex(IndexedGraph::vertex_property_type(0), g);
            vnames.push_back(0);
            assert(u == vnames.size() - 1);
          }
        
        std::vector<int>::iterator vit = std::find(vnames.begin(), vnames.end(), li->v);
        if( vit != vnames.end())
          {
            v = (vit - vnames.begin());
            std::cout << "v From vec " << v <<std::endl;
          }
        else
          {
            v = add_vertex(IndexedGraph::vertex_property_type(li->v), g);
            std::cout << "v To   vec " << v <<std::endl;
            vnames.push_back(li->v);
            assert(v == vnames.size() - 1);
          }
        
        Edge ee = add_edge (u, v, (-1)*(li->id), g).first; 
        
        // Set to negative to mark parallel edges not included in map
        ecm[ee] = -1;
        
        std::cout << "Added edge with index " << eim[ee] << " and component " << ecm[ee] << std::endl;
            
        std::cout << "Leg " << li->type << " added" << std::endl;
            
      }

    

    for(std::vector<Prop>::const_iterator pi = dr.props.begin(); pi != dr.props.end(); ++pi)
      {
        VD u,v;
        std::vector<int>::iterator vit = std::find(vnames.begin(), vnames.end(), pi->u);

        // One-loop tadpoles not added to graph
        // They are stored in list "self_edges"
        if(pi->u == pi->v)
          self_edges.push_back(pi->id);
        // All other edges added normally
        else                    
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
            ecm[ee] = -1;

            std::cout << "Edge " << pi->type << " added" << std::endl;
            // }
          }
      }
    

    // ==================================================================
    // 
    //    Tadpole Graph prepared filling subgraph information
    // 
    
    property_map < Graph_t, edge_component_t >::type component = get(edge_component, g);
    
    std::size_t num_comps = biconnected_components(g, component);

    subgraphs.resize(num_comps, std::pair<std::vector<int>, bool>(std::vector<int>(),false));
    // By default all graphs are tree
    // count_loops.resize(num_comps, 0);
    // std::vector<size_t> count_edges;

    
    count_verts.resize(num_comps);
    // count_edges.resize(num_comps, 0);

    std::cerr << "Found " << num_comps << " biconnected components.\n";
    
    graph_traits < Graph_t >::edge_iterator ei, ei_end;
    graph_traits < Graph_t >::out_edge_iterator pei, pei_end;
    
    for (tie(ei, ei_end) = edges(g); ei != ei_end; ++ei)
      {
        std::cout << get(edge_index, g, *ei) << " ::: ";
        std::cout << (char)(source(*ei, g) + 'A') << " -- " 
                  << (char)(target(*ei, g) + 'A');
        
        

        if (component[*ei] < 0) // Edge has parallel
          {
            int par_comp = -1;
            for (tie(pei, pei_end) = edge_range(source(*ei, g), target(*ei, g), g); pei != pei_end; ++pei)
              {
                std::cout << " par to (" << source(*pei, g) << "," << target(*pei, g) << ")" << get(edge_index, g, *pei) << std::endl;
                par_comp = std::max(par_comp,component[*pei]);
              }
            component[*ei] = par_comp;
          }
        std::cout << "[label=\"" << component[*ei] << "\"]\n";
        
        // If components are numerated from 0 to num_components
        // we add edges to [i-th] vector 
        std::cout << "Edge id " << get(edge_index, g, *ei) << std::endl;
        subgraphs[component[*ei]].first.push_back(get(edge_index, g, *ei));
        
        count_verts[component[*ei]].insert(source(*ei, g));
        count_verts[component[*ei]].insert(target(*ei, g));
        
        // If external legs in subgraph it is not a tadpole
        if(get(edge_index, g, *ei) < 0 ) subgraphs[component[*ei]].second = true;


      }

    // 
    // 
    // ======================================================================================== 


    std::vector<VD> art_points;
    articulation_points(g, std::back_inserter(art_points));
    std::cerr << "Found " << art_points.size() << " articulation points.\n";
    
    snails = art_points.size();
    
    
 
    for (std::size_t i = 0; i < art_points.size(); ++i) {
      std::cout << (char)(art_points[i] + 'A') 
                << " [ style=\"filled\", fillcolor=\"red\" ];" 
                << std::endl;
    }


           
    // dummy_visitor vis(3);
    
    // depth_first_search(g, visitor(vis));
    
 
  }
  
  size_t numTadpoles() const
  {
    return snails;
  }
  
  const std::vector<size_t>& tad1l() const
  {
    return self_edges;
  }

  const SubgraphIds& getSubgraphs() const
  {
    return subgraphs;
  }

  const std::set<int>& verts(size_t i) const
  {
    return count_verts[i]; 
  } 
};

#endif  // __TADPOLE_HPP__
