package org.agoravoting.lfa
 
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
 
import scala.util.Random
 
/**
 * Application that calculates the percentage of electors represented when
 * multiple questions are to be answered and each voter answers to only one,
 * but the voters have their vote delegated in other using multiple
 * non-transitive vote delegation.
 *
 * A voter is a vertex, a delegation is an edge. We create a random delegation
 * graph and calculate the "participation" using different number of average
 * number of delegates per voter.
 *
 * Returns a csv file.
 */
object NonTransitiveSampler extends Application {
    // number of voters
    val vertices = 500
 
    // abstention rate
    val absRate = 0.3
 
    // iterates with different edge vertex ratio
    ratioTrend(0, 11.0, vertices)
 
    /**
     * Calculate the representativity with random assigment of questions to
     * voters, using vote delegation, with a varying number of average
     * number of delegates per voter in the graph, and exports it to a CSV
     */
    def ratioTrend(ratioMin: Double, ratioMax: Double, vertices: Int,
                   inc: Double = 1) = {
        val ratios = ratioMin until (ratioMax, inc)
        val max = 0.01 * vertices
        val values = ratios.map{ r =>
            val graph = getRandomGraph(vertices, r)
            List(r, getCoverage(graph, 1) / max,
                    getCoverage(graph, 2) / max,
                    getCoverage(graph, 5) / max,
                    getCoverage(graph, 10) / max,
                    getCoverage(graph, 20) / max,
                    getCoverage(graph, 50) / max,
                    getCoverage(graph, 100) / max
            )
        }
 
        exportList(values, "ratio-score")
    }
 
    /**
     * Generates a random graph with a number of vertices and an average
     * number of edges per vertex
     */
    def getRandomGraph(vertices: Int, edgeVertexRatio: Double) = {
        val graph = new SimpleDirectedGraph[Object, PublicEdge](
            classOf[PublicEdge])
 
        val generator = new RandomGraphGenerator[Object, PublicEdge](
            vertices, (vertices * edgeVertexRatio).toInt)
 
        val vFactory = new ClassBasedVertexFactory(classOf[Object])
        generator.generateGraph(graph, vFactory, null)
 
        graph
    }
 
    /**
     * Given a graph of voters as vertices and delegations as edges, assigns
     * each voter (vertex) one question number, and calculate the average
     * coverage of a question, given that each vertex might either directly
     * answer a question it has been assigned, or it might have answered it
     * via a delegate, using non-transitive multiple vote delegation.
     */
    def getCoverage(graph: SimpleDirectedGraph[Object, PublicEdge],
                    questions: Int) = {
        // convert vertexSet to an (ordered) List
        val vertexList = graph.vertexSet.toList
 
        // assign one question to answer to each voter/vertex
        val assignments = vertexList.map{ vertex =>
            if (Random.nextDouble < absRate)
                -1
            else
                Random.nextInt(questions)
        }.toList
 
        // calculate coverage of each question. coverage is the number of
        // direct or indirect votes, i.e. number of responses
        (0 to questions-1).flatMap{ i =>
 
            // calculate coverage of this question: we go through each vertex,
            // and see if it has been assigned to answer to this question, or
            // if a neighbour vertex (i.e. a delegate) has
            vertexList.zipWithIndex.map{case(vertex, j) =>
                if (assignments.get(j) == i) 1
                else {
                    var incoming = graph.incomingEdgesOf(vertex)
                    incoming.map{edge =>
                        var src = edge.getPublicSrc
                        assignments.get(vertexList.indexOf(src)) == i
                    }.view.filter(_ == true).toSet.size
                }
            }
        }.view.reduce(_ + _)/questions
    }
 
    /**
     * exports a list of lists to a CSV
     */
    def exportList[T](lines: Seq[Seq[T]], fileName: String,
                      header: String = "") = {
        val pw = new java.io.PrintWriter(fileName + ".csv" , "UTF-8")
        // pw.write("id," + fileName + "\n")
        if(header.length > 0) pw.write(header + "\n")
        lines.foreach( lines => pw.write(lines.mkString(",") + "\n"))
        pw.close
    }
}
 
class PublicEdge extends DefaultEdge {
    def getPublicSrc() = {
        getSource
    }
}