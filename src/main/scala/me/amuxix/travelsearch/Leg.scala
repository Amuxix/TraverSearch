package me.amuxix.travelsearch

case class Leg(start: City, end: City, price: Int, transport: Transport) {
  def write: String = s"Take the $transport from $start to $end, it will cost you: $price"
}
