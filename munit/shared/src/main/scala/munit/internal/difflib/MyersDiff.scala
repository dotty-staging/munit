package munit.internal.difflib

import java.util

class MyersDiff[T](equalizer: Equalizer[T])
    extends munit.internal.difflib.DiffAlgorithm[T] {
  def this() = this(Equalizer.default[T])
  override def diff(
      original: util.List[T],
      revised: util.List[T]
  ): munit.internal.difflib.Patch[T] = {
    try {
      buildRevision(buildPath(original, revised), original, revised)
    } catch {
      case e: DifferentiationFailedException =>
        e.printStackTrace()
        new munit.internal.difflib.Patch[T]()
    }
  }
  private def buildRevision(
      _path: PathNode,
      orig: util.List[T],
      rev: util.List[T]
  ): munit.internal.difflib.Patch[T] = {
    var path: PathNode|Null = _path
    val patch = new munit.internal.difflib.Patch[T]
    if (path.nn.isSnake) path = path.nn.prev
    while (
      path != null &&
      path.prev != null &&
      path.prev.nn.j >= 0
    ) {
      if (path.isSnake)
        throw new IllegalStateException(
          "bad diffpath: found snake when looking for diff"
        )
      val i = path.i
      val j = path.j
      path = path.prev.nn
      val ianchor = path.i
      val janchor = path.j
      val original =
        new munit.internal.difflib.Chunk[T](
          ianchor,
          copyOfRange(orig, ianchor, i)
        )
      val revised =
        new munit.internal.difflib.Chunk[T](
          janchor,
          copyOfRange(rev, janchor, j)
        )
      val delta: munit.internal.difflib.Delta[T] =
        if (original.size == 0 && revised.size != 0) {
          new munit.internal.difflib.InsertDelta[T](original, revised)
        } else if (original.size > 0 && revised.size == 0) {
          new munit.internal.difflib.DeleteDelta[T](original, revised)
        } else {
          new munit.internal.difflib.ChangeDelta[T](original, revised)
        }
      patch.addDelta(delta)
      if (path.isSnake) {
        path = path.prev
      }
    }
    patch
  }

  private def copyOfRange(original: util.List[T], fromIndex: Int, to: Int) =
    new util.ArrayList[T](original.subList(fromIndex, to))

  def buildPath(
      orig: util.List[T],
      rev: util.List[T]
  ): PathNode = {

    val N = orig.size()
    val M = rev.size()

    val MAX = N + M + 1
    val size = 1 + 2 * MAX
    val middle = size / 2
    val diagonal = new Array[PathNode|Null](size)

    diagonal(middle + 1) = new Snake(0, -1, null)
    var d = 0
    while (d < MAX) {
      var k = -d
      while (k <= d) {
        val kmiddle = middle + k
        val kplus = kmiddle + 1
        val kminus = kmiddle - 1
        var prev: PathNode|Null = null
        var i = 0
        if ((k == -d) || (k != d && diagonal(kminus).nn.i < diagonal(kplus).nn.i)) {
          i = diagonal(kplus).nn.i
          prev = diagonal(kplus)
        } else {
          i = diagonal(kminus).nn.i + 1
          prev = diagonal(kminus)
        }
        diagonal(kminus) = null // no longer used

        var j = i - k
        var node: PathNode = new DiffNode(i, j, prev)
        // orig and rev are zero-based
        // but the algorithm is one-based
        // that's why there's no +1 when indexing the sequences
        while (
          i < N &&
          j < M &&
          equalizer.equals(orig.get(i), rev.get(j))
        ) {
          i += 1
          j += 1
        }
        if (i > node.i) {
          node = new Snake(i, j, node)
        }
        diagonal(kmiddle) = node
        if (i >= N && j >= M) {
          return diagonal(kmiddle).nn
        }

        k += 2
      }
      diagonal(middle + d - 1) = null
      d += 1
    }
    // According to Myers, this cannot happen
    throw new DifferentiationFailedException("could not find a diff path")
  }
}
