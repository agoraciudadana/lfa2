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
import scala.collection.SortedMap

import scala.util.Random

class Lfa2(val questions: Int, graph: SimpleDirectedGraph[Object, DefaultEdge]) extends Pause {
    // calculate the transitive closure
    
    // val tc = graph.clone().asInstanceOf[SimpleDirectedGraph[Object, DefaultEdge]]    
    // println("calculating transitive closure..")
    // TransitiveClosure.INSTANCE.closeSimpleDirectedGraph(tc)    
    
    // calculate the distance matrix
    print("calculating shortest paths (floyd-warshall)..")
    val fd = new FloydWarshallShortestPaths[Object, DefaultEdge](graph)
    println("ok")
    
    val vmap = graph.vertexSet.zipWithIndex.toMap  
    val vmapReverse = graph.vertexSet.zipWithIndex.map(_.swap).toMap                        
    
    print("calculating source map..")
    // calculate the source map
    val distances = for {
        v1 <- graph.vertexSet.toList.par
        v2 <- graph.vertexSet.toList
    } yield( (v2, v1, fd.shortestDistance(v1, v2) ))
    print("collecting..")
    
    val incomingMap = distances.filter(t => !t._3.isInfinite && (t._1 != t._2) && t._3 < 4).view.groupBy(_._1).map( x => vmap(x._1) -> x._2.map( y => (vmap(y._2) -> y._3) ) ).toMap
    println("ok")
    
    /* val firstVertex = graph.vertexSet.toIndexedSeq(0)
    println("node is " + vmap(firstVertex))
    val incoming = fd.getShortestPaths(firstVertex).map( x => vmap(x.getEndVertex) ).sortWith(_ < _)
    println("*** " + incoming);     
    val incoming2 = tc.incomingEdgesOf(firstVertex).map( x => vmap(tc.getEdgeSource(x))).toList.sortWith(_ < _)
    println(" *** " + incoming2); 
    println(" *** " + incomingMap.getOrElse(vmap(firstVertex), Nil))
    
    */
    
    
    // println( sources.toList.map( x=> fd.shortestDistance(x._1, x._2) ))           
    
    
    def randomState(questionsPerVertex: Int = 1) = {
        var assigned = Map[(Object, Int), Boolean]()
        val assignments = Array.fill(questions)(Set[Int]()).toIndexedSeq
        val tAssignments = Array.fill(questions)(SortedMap[Int, SortedMap[Double, Int]]()).toIndexedSeq
        
        var state = State(assignments, tAssignments)        
        for(i <- 1 to questionsPerVertex) {            
            vmap.keys.foreach { v =>
                var random = Random.nextInt(assignments.length)
                while(assigned.contains((v, random))) random = Random.nextInt(assignments.length)
                assigned = assigned + ((v, random) -> true)
                state = state.assign(v, random)
            }
        }
        
        state
    }       
    
    // hill climb
    def search(state: State, iterations: Int, jumpSize: Int, adaptive: Boolean = false) = {
        var next = state
        for (i <- 0 to iterations) {                        
            val j = if(adaptive) {
                (jumpSize * (iterations - i) / iterations)                
            }
            else jumpSize
            
            val newState = jump(next, j)            
            
            val newScore = score(newState)
            val oldScore = score(next)
            
            if(newScore > oldScore) {
                next = newState
                print(">")
            }            
            print(".")
        }        
        next
    }
    
    // simulated annealing
    def searchSA(state: State, iterations: Int, jumpSize: Int, adaptive: Boolean = false) = {
        var temp = 4.0
        var cooling = 0.9996
        var next = state
        var best = state
        var bestScore = score(state)
        
        for (i <- 0 to iterations) {
            val j = if(adaptive) {
                (jumpSize * (iterations - i) / iterations)                
            }
            else jumpSize
            
            val newState = jump(next, j)

            val newScore = score(newState)
            val oldScore = score(next)                        
            val prob = saProbability(oldScore, newScore, temp)
                                    
            if(prob > math.random) {
                next = newState
                if(newScore > oldScore) print(">")
                else if(newScore < oldScore) print("<")
            }
            
            if(newScore > bestScore) {
                bestScore = newScore
                best = newState
            }
            
            temp = temp * cooling            
            // temp = 50 * (iterations - i) / iterations
            print(".")                 
        }        
        // next
        best
    }
    
    // probability for simulated annealing
    private def saProbability(oldScore: Double, newScore: Double, temp: Double) = {
        val diff = newScore - oldScore
        if(diff > 0) {
            1.0
        }
        else {            
            math.exp(diff / temp)
            // println(" p = " + p + " diff = " + diff + " t = " + temp)            
        }
    }
    
    private def jump(state: State, jumpSize: Int) = {
        var next = move(state)
        for(i <- 1 to jumpSize - 1) {            
            next = move(next)
        }
        next
    }
    
    private def move(state: State) = {
        // choose source question
        var source = Random.nextInt(state.assignments.length)                             
        while(state.assignments(source).size == 0) source = Random.nextInt(state.assignments.length)                             
        // choose target question
        var target = source
        while(target == source) target = Random.nextInt(state.assignments.length)
        // choose random vertex from source
        val vertices = state.assignments(source).toArray        
        val randomVertex = Random.nextInt(vertices.length)
        val vertex = vertices(randomVertex)
        val vertexObject = vmapReverse(vertex)
                                       
        // move vertex from source to target
        state.unassign(vertexObject, source).assign(vertexObject, target)
    }
    
    // state score, does not consider overloaded voters
    def score(state: State) = {        
        state.tAssignments.map(scoreQuestion(_)).reduce(_ + _)
    }
    // question score
    def scoreQuestion(question: SortedMap[Int,SortedMap[Double, Int]]) = {
        if(question.isEmpty) 0
        else question.values.map(_.lastOption.getOrElse(0.0,0)._1).reduce(_ + _)
    }
    // state score, does not consider branch lengths
    def scoreRaw(state: State) = {        
        state.tAssignments.map(scoreQuestionRaw(_)).reduce(_ + _)
    }
    // question score, ignore branch lengths
    def scoreQuestionRaw(question: SortedMap[Int,SortedMap[Double, Int]]) = {
        if(question.isEmpty) {            
            0.0
        }
        else question.values.map{ lengths =>
            if(lengths.isEmpty) 0
            else 1.0
        }.reduce(_ + _)
    }
    // score based on delegation path length
    def getDistanceScore(distance: Double) = {
        if(distance > 3) 0.0
        else 1 / (distance + 1)
    }
    
    def maxScore(questionsPerVertex: Int) = {
        (questionsPerVertex * graph.vertexSet.size * getDistanceScore(0)) + ((questions - questionsPerVertex) * graph.vertexSet.size * getDistanceScore(1.0))
    }
    val maxScoreRaw = questions * graph.vertexSet.size    
    
    // a node of the search tree
    case class State(assignments: IndexedSeq[Set[Int]], tAssignments: IndexedSeq[SortedMap[Int,SortedMap[Double, Int]]], debug: String = "") {                        
        
        // create a new state by assigning a vertex to a question
        def assign(vertex: Object, question: Int) = {
            val index = vmap(vertex)
            
            val a = assignments(question) + index
                    
/*
                    val incoming = tc.incomingEdgesOf(vertex)        
            
            // val updates = incoming.map(x => (vmap(tc.getEdgeSource(x)), 1.0)).toList
            val updates = incoming.map{x => 
                val source = tc.getEdgeSource(x)
                val distance = fd.shortestDistance(source, vertex)                
                (vmap(tc.getEdgeSource(x)), getDistanceScore(distance))
            }.filter(u => (u._2 != 0) && (u._1 != index)).toList
  */          
            val incoming = incomingMap.getOrElse(index, Nil)
            val updates = incoming.map{ x => 
                (x._1, getDistanceScore(x._2))
            }.filter(u => (u._2 != 0) && (u._1 != index)).toList                       


            val t = addValues((index, 1.0) :: updates, tAssignments(question))

            State(assignments.updated(question, a), tAssignments.updated(question, t), index + " -> " + question)
        }
        
        // create a new state by unassigning a vertex to a question
        def unassign(vertex: Object, question: Int) = {
            val index = vmap(vertex)
            
            if(!assignments(question).contains(index)) throw new Exception("removing from A when value does not exist")
            
            val a = assignments(question) - index
            
/*            val incoming = tc.incomingEdgesOf(vertex)
            
            // val updates = incoming.map(x => (vmap(tc.getEdgeSource(x)), 1.0)).toList

            val updates = incoming.map{x => 
                val source = tc.getEdgeSource(x)
                val distance = fd.shortestDistance(source, vertex)
                (vmap(tc.getEdgeSource(x)), getDistanceScore(distance))
            }.filter(u => (u._2 != 0) && (u._1 != index)).toList                       
  */               
            val incoming = incomingMap.getOrElse(index, Nil)
            val updates = incoming.map{ x => 
                (x._1, getDistanceScore(x._2))
            }.filter(u => (u._2 != 0) && (u._1 != index)).toList                       

            val t = removeValues((index, 1.0) :: updates, tAssignments(question))

            State(assignments.updated(question, a), tAssignments.updated(question, t), index + " <- " + question)
        }               
        
        // update the t assignment map structure with current values (preserves memory of previous values)        
        private def addValues(values: List[(Int, Double)], map: SortedMap[Int,SortedMap[Double, Int]]): SortedMap[Int,SortedMap[Double, Int]] = values match {
            case x :: xs => {
                if(map contains x._1) {
                    val old = map(x._1)
                    val newValue = if(old contains x._2) {
                        val o = old(x._2)
                        (old - x._2) + (x._2 -> (o + 1))
                    }
                    else {
                        old + (x._2 -> 1)
                    }                                                          
                    addValues(xs, (map - x._1) + (x._1 -> newValue))
                }
                else {
                    addValues(xs, map + (x._1 -> SortedMap(x._2 -> 1)))
                }
            }
            case Nil => map
        }
        private def removeValues(values: List[(Int, Double)], map: SortedMap[Int,SortedMap[Double, Int]]): SortedMap[Int,SortedMap[Double, Int]] = values match {
            case x :: xs => {
                if(map contains x._1) {
                    val old = map(x._1)
                    val newValue = if(old contains x._2) {
                        val o = old(x._2)
                        if(o - 1 == 0) {
                            old - x._2
                        }
                        else {
                            (old - x._2) + (x._2 -> (o - 1))
                        }
                    }
                    else {
                        old - x._2
                    }                                                          
                    removeValues(xs, (map - x._1) + (x._1 -> newValue))
                }
                else {
                    throw new Exception("removing from T when value does not exist")                    
                }
            }
            case Nil => map
        }      
        // gets assignments in the form (vertex -> (question1, question2, question3..))
        def getAssignments = {
            val tuples = assignments.zipWithIndex.map(_.swap).flatMap( x => x._2.map(_ -> x._1) )
            // grouped by vertex
            tuples.groupBy(_._1).map(x => x._1 -> x._2.map(_._2))
        }
        // gets the vertices that are assigned to a question, transitively
        def sourcesForQuestion(question: Int) = {
            val vertices = assignments(question).toList            
            val vObjects = vertices.map(vmapReverse(_))            
            val sources = vObjects.map(vertex => (vmap(vertex), incomingMap(vmap(vertex))))
            sources
        }
        
        def print = {
            println("* assignments " + assignments.zipWithIndex.map(_.swap) + " (score = " +  score(this) + ")")
            // println("* assignments " + getAssignments + " (score = " +  score(this) + ")")
            println("* scores " + tAssignments.zipWithIndex.map(x => (x._2, scoreQuestion(x._1))))
            println("* tassignments ")
            tAssignments.zipWithIndex.foreach { x =>
                println(x._2 + ": " + x._1)
            }            
        }
    }       
}