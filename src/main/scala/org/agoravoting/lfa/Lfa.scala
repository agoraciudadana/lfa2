package org.agoravoting.lfa

import com.tinkerpop.furnace.generators._
import com.tinkerpop.blueprints.Graph
import com.tinkerpop.blueprints.impls.tg.TinkerGraph
import com.tinkerpop.blueprints.util.io.graphml.GraphMLWriter
import com.tinkerpop.blueprints.Vertex

import java.io._
import org.jgrapht._
import org.jgrapht.generate._
import org.jgrapht.graph._
import org.jgrapht.traverse._
import org.jgrapht.ext._
import org.jgrapht.alg._

import collection.JavaConversions._
import scala.collection.mutable.ArrayBuffer
import scala.annotation.tailrec

class Lfa(val questions: Int, graph: SimpleDirectedGraph[Object, DefaultEdge]) extends Pause {
    // calculate the transitive closure
    val tc = graph.clone().asInstanceOf[SimpleDirectedGraph[Object, DefaultEdge]]
    TransitiveClosure.INSTANCE.closeSimpleDirectedGraph(tc)
    
    // calculate the distance matrix (currently unused)
    val fd: FloydWarshallShortestPaths[Object, DefaultEdge] = new FloydWarshallShortestPaths[Object, DefaultEdge](graph)
    val vmap: Map[Object, Int] = graph.vertexSet.zipWithIndex.toMap       
           
    /* recursive code
    def go = recurse(List(start))
    
    // calculate all child nodes and recurse greedily into the highest scoring one
    def recurse(states: List[State]) : List[State] = {
        if(states.size == 0) {
            states
        }
        else {
            val sorted = states.map{ s => (s, score(s)) }.sortWith(_._2 > _._2)                        
            // pause(sorted.map(x => x._2 + " " + x._1.debug))
            val first = sorted(0)._1
            // first :: recurse(first.forward)
            recurse(first.forward)
        }
    }*/
    
    def go = {
        // init assignments
        val assignments = Array.fill(questions)(Set[Int]()).toIndexedSeq
        val tAssignments = Array.fill(questions)(Map[Int, Double]()).toIndexedSeq
    
        // the start state with no assignments        
        val start = State(assignments, tAssignments, vmap)
        loop(start)
    }
    
    def loop(state: State) : List[State] = {
        var next = List(state)
        var current = List[State]()
        
        while(next.length > 0) {
            val sorted = next.map{ s => (s, score(s)) }.sortWith(_._2 > _._2)                        
            // pause(sorted.map(x => x._2 + " " + x._1.debug))
            val first = sorted(0)._1
            // first :: recurse(first.forward)
            current = next
            next = first.forward
        }
        current
    }
    
    // state score, does not consider overloaded voters nor branch lengths
    def score(state: State) = {        
        state.tAssignments.map(scoreQuestion(_)).reduce(_ + _)
    }
    // question score
    def scoreQuestion(question: Map[Int,Double]) = {
        if(question.isEmpty) 0
        else question.values.reduce(_ + _)
    }       
    
    // a node of the search tree
    case class State(assignments: IndexedSeq[Set[Int]], tAssignments: IndexedSeq[Map[Int,Double]], vertices: Map[Object, Int], debug: String = "") {
        // calculate all the child nodes from this node
        
        def forward: List[State] = {
            println("calculating " + (vertices.keys.size * assignments.length) + " nodes")
            if(vertices.size > 0) {
                val keys = vertices.keys.par
            
                val ret = for {
                    v <- keys
                    q <- 0 to assignments.length - 1
                } yield( assign(v, q) )
                
                ret.toList
            }
            else Nil            
        }
        
        // calculate one child node
        def assign(vertex: Object, question: Int) = {
            val index = vmap(vertex)
            
            val a = assignments(question) + index
                    
            val incoming = tc.incomingEdgesOf(vertex)        
            val updates = incoming.map(x => (vmap(tc.getEdgeSource(x)), 1)).toList
            // val distance = fd.shortestDistance            
            val t = updateT((index, 1) :: updates, tAssignments(question))
            
            State(assignments.updated(question, a), tAssignments.updated(question, t), vertices - vertex, index + " -> " + question)
        }
        
        // update the t assignment map structure with current values (fixed to 1.0 for now)
        // if the value for the given voter is higher, it is overwritten
        private def updateT(values: List[(Int, Int)], map: Map[Int,Double]): Map[Int,Double] = values match {
            case x :: xs => {
                if(map contains x._1) {
                    val old = map(x._1)
                    val newValue = if(x._2 > old) x._2 else old
                    updateT(xs, (map - x._1) + (x._1 -> newValue))
                }
                else {
                    updateT(xs, map + (x._1 -> x._2))
                }
            }
            case Nil => map
        }        
    }
}