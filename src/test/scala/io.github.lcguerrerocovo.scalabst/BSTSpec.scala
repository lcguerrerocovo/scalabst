package io.github.lcguerrerocovo.scalabst

import java.util.concurrent.{TimeUnit, ThreadLocalRandom}

import org.scalatest._

/**
  * Created by luisguerrero
  */
class BSTSpec extends FlatSpec with Matchers {

  var tree = BinarySearchTree()
  tree += ((4L,"53fe4474-907d-4f90-9179-766e3e446884"))
  tree += ((2L,"54a1e348-aea3-4a28-8689-a53e68c9148a"))
  tree += ((1L,"c23c96dc-4d8b-40f8-8a10-a5f7972c29a1"))
  tree += ((3L,"5798326c-0fe1-4884-91cd-e06f102af775"))
  tree += ((5L,"b13ca51c-2d22-4873-8e6c-644a16f1be0c"))

  val printTree =   List("       2        ",
                         "      / \\       ",
                         "   1       4    ",
                         "  / \\     / \\   ",
                         "         3   5  ")

  it should "be able to convert a BST into a list of lists where each list" +
    "respresents a depth level in the BST and empty nodes are equal to -1" in {
    tree.root.toList should equal (
      List(
        List((3,2,List("54a1e348-aea3-4a28-8689-a53e68c9148a"))),
        List((2,4,List("53fe4474-907d-4f90-9179-766e3e446884")),
          (1,1,List("c23c96dc-4d8b-40f8-8a10-a5f7972c29a1"))),
        List((1,5,List("b13ca51c-2d22-4873-8e6c-644a16f1be0c")),
          (1,3,List("5798326c-0fe1-4884-91cd-e06f102af775")),
          (0,-1,List()), (0,-1,List()))))
  }

  it should "print the tree" in {
    tree.root.toPretty('t') should equal (printTree mkString "\n")
  }

  it should "rotate right on grandparent when inserting a node which is less than " +
    "parent and less than grandparent and balance is broken" in {
    val rr = BinarySearchTree()
    rr += ((4L,"53fe4474"))
    rr += ((2L,"54a1e348"))
    rr += ((1L,"c23c96dc"))
    rr.root.toList should equal (
      List(List((2,2,List("54a1e348"))), List((1,4,List("53fe4474")),
        (1,1,List("c23c96dc"))))
    )
  }

  it should "rotate left on parent and rotate right on grandparent when inserting a " +
    "node which is greater than parent and less than grandparent and balance is broken" in {
    val lr = BinarySearchTree()
    lr += ((4L,"53fe4474"))
    lr += ((2L,"54a1e348"))
    lr += ((3L,"c23c96dc"))
    lr.root.toList should equal (
      List(List((2,3,List("c23c96dc"))), List((1,4,List("53fe4474")),
        (1,2,List("54a1e348"))))
    )
  }

  it should "rotate left on grandparent when inserting a node which is greater than " +
    "parent and greater than grandparent and balance is broken" in {
    val ll = BinarySearchTree()
    ll += ((4L,"53fe4474"))
    ll += ((6L,"54a1e348"))
    ll += ((8L,"c23c96dc"))
    ll.root.toList should equal (
      List(List((2,6,List("54a1e348"))), List((1,8,List("c23c96dc")),
        (1,4,List("53fe4474"))))
    )
  }

  it should "rotate right on parent and rotate left on grandparent when inserting a " +
    "node which is greater than grandparent and less than parent and balance is broken" in {
    val rl = BinarySearchTree()
    rl += ((4L,"53fe4474"))
    rl += ((6L,"54a1e348"))
    rl += ((5L,"c23c96dc"))
    rl.root.toList should equal (
      List(List((2,5,List("c23c96dc"))), List((1,6,List("54a1e348")),
        (1,4,List("53fe4474"))))
    )
  }

  it should "search efficiently for operations corresponding to timestamp " in {
    val t = BinarySearchTree()
    t += ((4L,"53fe4474"))
    t += ((6L,"54a1e348"))
    t += ((5L,"c23c96dc"))
    t += ((5L,"5798326c"))
    t.root.search(5) should equal (List(List("5798326c", "c23c96dc")))
  }

  it should "search efficiently for operations corresponding to a range of timestamps " in {
    val t = BinarySearchTree()
    t += ((1L,"13fe4474"))
    t += ((2L,"24a1e348"))
    t += ((3L,"323c96dc"))
    t += ((4L,"4798326c"))
    t += ((5L,"553e68c9"))
    t += ((6L,"644a16f1"))
    t += ((7L,"766e3e44"))
    t.root.search(2L,6L) should equal (List(List("24a1e348"), List("323c96dc"),
      List("4798326c"), List("553e68c9"), List("644a16f1")))
  }

  it should "search efficiently and correctly even if root node outside range" in {
    val t = BinarySearchTree()
    t += ((1L,"13fe4474"))
    t += ((5L,"54a1e348"))
    t += ((7L,"723c96dc"))
    t += ((11L,"1198326c"))
    t += ((21L,"213e68c9"))
    t += ((40L,"404a16f1"))
    t += ((59L,"59868959"))
    t += ((61L,"61907d44"))
    t += ((73L,"73766e3e"))
    t += ((80L,"805418fe"))
    t += ((100L,"100d7652"))
    t.prettyPrint('t')

    t.root.search(15L,79L) should equal (List(List("213e68c9"), List("404a16f1"),
      List("59868959"), List("61907d44"), List("73766e3e")))
  }
}
