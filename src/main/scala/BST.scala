object Interview extends App {

  sealed trait Tree
  case object EmptyTree extends Tree
  case class NonEmptyTree(key: Int, value: String, left: Tree, right: Tree) extends Tree

  val exampleTree =
    NonEmptyTree(3, "Jakko",
    NonEmptyTree(2, "Matt",
      NonEmptyTree(1, "Bob",
        EmptyTree,
        EmptyTree),
      EmptyTree),
    NonEmptyTree(7, "Joe",
      EmptyTree,
    NonEmptyTree(11, "Emily",
      NonEmptyTree(8, "Eleri",
        EmptyTree,
        EmptyTree),
      EmptyTree)))

  sealed trait Comparison
  case object Equals extends Comparison
  case object Less extends Comparison
  case object Greater extends Comparison

  def find(tree: Tree, inputKey: Int): Option[String] = {
    tree match {
      case EmptyTree => None
      case NonEmptyTree(key, value, left, right) =>
        compare(inputKey, key) match {
          case Equals => Some(value)
          case Less => find(left, inputKey)
          case Greater => find(right, inputKey)
        }
      }
    }

  def compare(a: Int, b: Int): Comparison =
    if (a == b) Equals
    else if (a < b) Less
    else Greater

  def valid(tree: Tree): Boolean = {
    tree match {
      case EmptyTree => true
      case NonEmptyTree(parentKey, value, left, right) => (left, right) match {
        case (EmptyTree, EmptyTree) => true
        case (EmptyTree, NonEmptyTree(childKey,_,_,_)) =>
         parentKey < childKey && valid(left)
        case (NonEmptyTree(childKey,_,_,_), EmptyTree) =>
          parentKey > childKey && valid(right)
        case (NonEmptyTree(leftChildKey, _, _, _), NonEmptyTree(rightChildKey, _, _, _)) =>
         parentKey > leftChildKey && parentKey < rightChildKey && valid(left) && valid(right)
        }
    }
  }

  println(find(exampleTree,7))
  println(find(exampleTree,1))
  println(find(exampleTree,20))
  println(find(exampleTree, -650))
  println(valid(exampleTree))

  }
