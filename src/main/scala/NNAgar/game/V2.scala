package NNAgar.game

case class V2(x: Double, y: Double) {
  def +(o: V2): V2 = V2(x + o.x, y + o.y)

  def -(o: V2): V2 = V2(x - o.x, y - o.y)

  def *(o: V2): V2 = V2(x * o.x, y * o.y)
  def *(o: Double): V2 = V2(x * o, y * o)

  def /(o: V2): V2 = V2(x / o.x, y / o.y)

  def **(ot: V2): Double = x * ot.x + y * ot.y

  def rotate(a: Double): V2 = V2(
    x * math.cos(a) - y * math.sin(a),
    x * math.sin(a) + y * math.cos(a))


  def length: Double = math.sqrt(x * x + y * y)

  def normalize: V2 = this / V2(length, length)

  def capLength(len: Double): V2 = if (length > len) this * (len / length) else this

  def to0twoPi(a: Double): Double = {
    val TWO_PI = math.Pi * 2d
    if (a < 0) a % (TWO_PI) + TWO_PI
    else if (a >= TWO_PI) a % TWO_PI
    else a
  }

  /** 0 -> 2 PI */
  def angleToOx: Double = to0twoPi(math.atan2(y, x))
  /** 0 -> 2 PI */
  def angle(ot: V2): Double = to0twoPi(math.atan2(ot.y - this.y, ot.x - this.x))
}
