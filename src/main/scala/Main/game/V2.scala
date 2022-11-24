package Main.game

case class V2(x: Double, y: Double) {
  def +(o: V2): V2 = V2(x + o.x, y + o.y)

  def -(o: V2): V2 = V2(x - o.x, y - o.y)

  def *(o: V2): V2 = V2(x * o.x, y * o.y)
  def *(o: Double): V2 = V2(x * o, y * o)

  def /(o: V2): V2 = V2(x / o.x, y / o.y)

  def **(ot: V2): Double = x * ot.x + y * ot.y

  def rotate(a: Double): V2 = V2(
    x * math.sin(a) + y * math.cos(a),
    x * math.cos(a) - y * math.sin(a))


  def length: Double = math.sqrt(x * x + y * y)
}
