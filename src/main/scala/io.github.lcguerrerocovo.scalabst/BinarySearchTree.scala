package io.github.lcguerrerocovo.scalabst

/**
  * Created by luisguerrero
  */

class BinarySearchTree {
  var root: BST = EmptyBST
  def +=(data: (Long,String)) = root = root.insert(root,data)
  def prettyPrint(value: Char) = println(root.toPretty(value))
  def search(min: Long, max: Long) = {
    if(min == Long.MinValue && max == Long.MaxValue)
      root.traverse
    else root.search(min,max)
  }
}

object BinarySearchTree {
  def apply(): BinarySearchTree = new BinarySearchTree()
}

trait BST {
  def isEmpty: Boolean
  def isRoot: Boolean

  def insert(parent: BST, data :(Long,String)): BST
  def search(ts: Long): List[List[String]]
  def search(min: Long, max: Long): List[List[String]]
  def removeLessThan(min: Long): BST
  def removeGreaterThan(min: Long): BST
  def traverse: List[List[String]]
  def rotateRight: BST
  def rotateLeft: BST
  def rotateLR: BST
  def rotateRL: BST
  val height: Int
  def hdiff: Int

  val depth: Int
  def toList: List[List[(Int,Long, List[String])]]
  def toPretty(value: Char): String

}

case class NonEmptyBST(parent: BST, left: BST, right: BST,height: Int, data: (Long,List[String])) extends BST {
  def isEmpty = false
  def isRoot = if(parent == EmptyBST) true else false
  lazy val depth = treeDepth
  def hdiff: Int = right.height - left.height

  def insert(parent: BST, data :(Long,String)): BST = {
    if(data._1 == this.data._1) NonEmptyBST(parent,left,right,height,(data._1,data._2 :: this.data._2))
    else if(data._1 < this.data._1) {
      val newLeft = left.insert(parent,data)
      NonEmptyBST(parent,newLeft,right,height max newLeft.height+1,this.data).balance
    }
    else {
      val newRight = right.insert(parent,data)
      NonEmptyBST(parent,left,newRight,height max newRight.height+1,this.data).balance
    }
  }

  def search(ts: Long) = {
    search(ts,ts)
  }

  def search(min: Long, max: Long): List[List[String]] = {
    removeLessThan(min).removeGreaterThan(max).traverse
  }

  def traverse: List[List[String]] = {
    left.traverse ::: (data._2 :: right.traverse)
  }

  def removeLessThan(min: Long): BST = {
    if(data._1 < min) right.removeLessThan(min)
    else NonEmptyBST(parent,left.removeLessThan(min),right,height,data)
  }

  def removeGreaterThan(max: Long): BST = {
    if(data._1 > max) left.removeGreaterThan(max)
    else NonEmptyBST(parent,left,right.removeGreaterThan(max),height,data)
  }

  def balance: BST = {
    hdiff match {
      case -2 => left.hdiff match {
        case 1 =>
          rotateLR
        case _ =>
          rotateRight
      }
      case 2 => right.hdiff match {
        case -1 =>
          rotateRL
        case _ =>
          rotateLeft
      }
      case _ => this
    }
  }

  /****
    *   tree rotation operations to maintain balance
    */

  def rotateRight: BST = {
    left match {
      case NonEmptyBST(p, l, r, h, d) => {
        val newRight = NonEmptyBST(left,r,right,r.height+1 max right.height+1 ,this.data)
        return NonEmptyBST(this.parent,l,newRight,l.height+1 max newRight.height+1,d)
      }
      case EmptyBST => return this
      }
  }

  def rotateLR: BST = {
    val newLeft = left.rotateLeft
    val node = NonEmptyBST(parent,newLeft,right,newLeft.height+1 max right.height+1,data)
    node.rotateRight
  }

  def rotateLeft: BST = {
    right match {
      case NonEmptyBST(p, l, r, h, d) => {
        val newLeft = NonEmptyBST(right,left,l,left.height+1 max l.height+1,this.data)
        return NonEmptyBST(this.parent,newLeft,r,newLeft.height+1 max r.height+1,d)
      }
      case EmptyBST => return this
    }
  }

  def rotateRL: BST = {
    val newRight = right.rotateRight
    val node = NonEmptyBST(parent,left,newRight,newRight.height+1 max left.height+1,data)
    node.rotateLeft
  }

  /****
    *   the following are utility methods for tree printing and testing only
    */

  def toList: List[List[(Int,Long, List[String])]] = {
    def extract(tree: BST): (BST,BST,(Int,Long,List[String])) = tree match {
      case NonEmptyBST(p,l,r,h,d) => (l,r,(h,d._1,d._2))
      case EmptyBST => (EmptyBST,EmptyBST,(0,-1,Nil))
    }
    def toList(tree: BST, level: Int, depth: Int, acc: List[(Int,Long,List[String])]): List[(Int,Long,List[String])] = {
      val (l,r,y) = extract(tree)
      if (level == depth) y :: acc
      else {
        val res = toList(l, level + 1, depth, acc)
        return toList(r, level + 1, depth, res)
      }
    }
    (for {
      i <- 1 to depth
    } yield toList(this,1,i,Nil)).toList
  }

  def treeDepth: Int = this match {
    case NonEmptyBST(p,EmptyBST,EmptyBST,h,d) => 1
    case NonEmptyBST(p,l,r,h,d) => 1 + (l.depth max r.depth)
  }

  def toPretty(value: Char): String  = {
    val lst = this.toList
    val printTree = for {
      i <- lst.indices
      d = lst.last.size/lst(i).size*2
      str = lst(i).reverse.map(x =>
        if (x._2 >= 0) {
          if(value=='t') s"${x._2}" else s"${x._1}"
        } else " ")
      node = str.map(y => " " * (d - y.length) + y + " " * d)
      brackets = str.map(y => {
        val (l,r) = if(y == " ") (" "," ") else ("/","\\")
        " " * (d - 2) + l + " " + r + " " * (d - 1)
      })
    } yield List((node mkString ""), (brackets mkString ""))
    (printTree.init.map(x => x.head + "\n" + x.last) mkString "\n") +
      "\n" + printTree.last(0)
  }
}

case object EmptyBST extends BST {
  def isRoot = false
  def isEmpty = true
  val depth = 0
  def toList = List(List((0,-1L,Nil)))
  def toPretty(value: Char) = ""
  def rotateRight = this
  def rotateLeft = this
  def rotateRL = this
  def rotateLR = this
  val height = 0
  val hdiff: Int = 0

  def insert(parent: BST, data: (Long,String)): BST
    = NonEmptyBST(parent,EmptyBST,EmptyBST,1,(data._1, data._2 :: Nil))

  def search(ts: Long) = Nil
  def search(min: Long, max: Long) = Nil

  def removeLessThan(min: Long) = EmptyBST
  def removeGreaterThan(min: Long) = EmptyBST
  def traverse = Nil

}