#ifndef __GRAPH_HPP__
#define __GRAPH_HPP__

#include "types.hpp"
#include <iostream>
#include <boost/graph/adjacency_list.hpp> 
#include <boost/graph/labeled_graph.hpp>
#include <boost/graph/connected_components.hpp>
using namespace boost; 

// Vertex name - possible negative index from eternal legs
typedef boost::adjacency_list<vecS, vecS, undirectedS, property<vertex_name_t, int>,property<edge_index_t, std::size_t> > IndexedGraph;

// typedef labeled_graph<AdjGraph, int> IndexedGraph;

typedef boost::graph_traits<IndexedGraph>::vertex_descriptor VD;

// Subgraph containing only fermion lines
// type = F and type = M.
// External legs are connected to vertex E

class FermionGraph
{
  IndexedGraph g;
  
  std::vector<int> vnames;
  std::map<int,VD> vidx;
  
  size_t fermion_lines;

  size_t open_lines;
  size_t closed_lines;

public:
  FermionGraph(const DiagramRecord& dr)
  {
    
    size_t v_idx_shift = dr.legs.size(); // Negative vertex index shift by number of eternal legs
    for(std::vector<Prop>::const_iterator pi = dr.props.begin(); pi != dr.props.end(); ++pi)
      {
        // Add only Dirac and Majorana fermion edges
        if((pi->type == "F") || (pi->type == "M"))
          {
            VD u,v;
            std::vector<int>::iterator vit = std::find(vnames.begin(), vnames.end(), pi->u);
            if(vit  != vnames.end())
              {
                u = (vit - vnames.begin());

                std::cout << "u From vec " << u <<std::endl;
              }
            else
              {
                u = add_vertex(IndexedGraph::vertex_property_type(pi->u), g);

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
                v = add_vertex(IndexedGraph::vertex_property_type(pi->v), g);
                std::cout << "v To   vec " << v <<std::endl;
                vnames.push_back(pi->v);
                assert(v == vnames.size() - 1);
              }
            
            add_edge (u, v, pi->id, g); 
            
            std::cout << "Edge " << pi->type << " added" << std::endl;
          }
      }



    std::cout << "Fermion flow graph has " << num_edges(g) << " edges from props and " << num_vertices(g) << " verts " << std::endl;
    
    for(std::vector<Leg>::const_iterator li = dr.legs.begin(); li != dr.legs.end(); ++li)
      {
        // Add only Dirac and Majorana fermion legs
        if((li->type == "F") || (li->type == "M"))
          {
            VD u,v;
            std::vector<int>::iterator vit = std::find(vnames.begin(), vnames.end(), li->u);
            if(vit  != vnames.end())
              {
                u = (vit - vnames.begin());
                
                std::cout << "u From vec " << u <<std::endl;
              }
            else
              {
                u = add_vertex(IndexedGraph::vertex_property_type(li->u), g);
                
                std::cout << "u To   vec " << u <<std::endl;
                vnames.push_back(li->u);
                assert(u == vnames.size() - 1);
              }
            
            vit = std::find(vnames.begin(), vnames.end(), li->v);
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
            
            add_edge (u, v, li->id, g); 
            
            std::cout << "Leg " << li->type << " added" << std::endl;
            
            //     std::cout << "Leg " << li->type << " added" << std::endl;
          }
        
      }
    std::cout << "Fermion flow graph has " << num_edges(g) << " edges from props and legs" << std::endl;
    
    std::vector<int> component(num_vertices(g));
    fermion_lines = connected_components(g, &component[0]);
    

    // By default all lines are closed
    std::vector<bool> line_is_closed(fermion_lines, true);
    for (int i = 0; i != component.size(); ++i)
      if(vnames[i] < 0) line_is_closed[component[i]] = false;


    std::cout << "Total number of components: " << fermion_lines << std::endl;
    for (int i = 0; i != component.size(); ++i)
      std::cout << "Vertex " << i <<" is in component " << component[i] << std::endl;
    std::cout << std::endl;


    open_lines = std::count(line_is_closed.begin(), line_is_closed.end(), false);
    closed_lines = std::count(line_is_closed.begin(), line_is_closed.end(), true);


    std::cout << "We have " << std::count(line_is_closed.begin(), line_is_closed.end(), true) << " closed loops" << std::endl; 

  }

  size_t open() const
  {
    return open_lines;
  }  

  size_t closed() const
  {
    return closed_lines;
  }  
  
};


#endif  // __GRAPH_HPP__
