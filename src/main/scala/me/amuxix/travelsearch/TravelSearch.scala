package me.amuxix.travelsearch

import scala.collection.{MapView, mutable}
import collection.immutable.ListMap
import me.amuxix.travelsearch.City._
import me.amuxix.travelsearch.Transport._
import me.amuxix.travelsearch.City

object TravelSearch {
  def main(args: Array[String]): Unit = {
    val inf = 1000000

    val boatPrices: Map[City, List[Int]] = ListMap(
      AbDendriel -> List( 0 , inf,  80, inf,  70, inf, inf, inf, inf, inf, 130,  90, 160, inf, inf),
      Ankrahmun  -> List(inf,  0 , inf, 100, 160,  90,  80, inf, inf, inf, inf, 150, 230, inf, inf),
      Carlin     -> List( 80, inf,  0 , inf, 110, inf, inf, inf, inf, 110, 110, 130, 185, inf, inf),
      Darashia   -> List(inf, 100, inf,  0 , inf, 200, 180, inf, inf, inf, inf,  60, 210, 110, 130),
      Edron      -> List( 70, 160, 110, inf,  0 , 170, 150, inf, inf, inf, 160,  40, inf, 100, inf),
      LibertyBay -> List(inf,  90, inf, 200, 170,  0 ,  50, inf, inf, inf, 180, 180, 275, inf, inf),
      PortHope   -> List(inf, 110, inf, 180, 150,  50,  0 , inf, inf, inf, 160, 160, 260, inf, inf),
      Roshamuul  -> List(inf, inf, inf, inf, inf, inf, inf,  0 , inf, inf, 210, inf, inf, inf, inf),
      Oramond    -> List(inf, inf, inf, inf, 110, inf, 200, inf,  0 , inf, 150, 130, inf,  60, 120),
      Svargrond  -> List(inf, inf, 110, inf, inf, inf, inf, inf, inf,  0 , 180, 150, inf, inf, inf),
      Thais      -> List(130, inf, 110, inf, 160, 180, 160, 210, 150, 180,  0 , 170, 200, inf, inf),
      Venore     -> List( 90, 150, 130,  60,  40, 180, 160, inf, inf, 150, 170,  0 , 185, 110, 130),
      Yalahar    -> List(160, 230, 185, 210, inf, 275, 260, inf, inf, inf, 200, 185,  0 , inf, inf),
      Krailos    -> List(inf, inf, inf, 110, 100, inf, inf, inf,  60, inf, inf, 110, inf,  0 ,  70),
      Issavi     -> List(inf, inf, inf,  80, inf, inf, inf, inf, 100, inf, inf,  80, inf,  80,  0 ),
    )

    val magicCarpetPrices: Map[City, List[Int]] = ListMap(
      Darashia   -> List( 0 ,  40,  60,  60,  60,  80, 100),
      Edron      -> List( 40,  0 ,  60,  60,  60,  70, 100),
      Farmine    -> List( 60,  60,  0 ,  60,  60,  70, 100),
      FemorHills -> List( 60,  60,  60,  0 ,  60,  70, 100),
      Svargrond  -> List( 60,  60,  60,  60,  0 ,  70, 100),
      Kazordoon  -> List( 40,  40,  60,  60,  60,  0 , 100),
      Issavi     -> List( 40,  60,  60,  60,  60,  70,  0 ),
    )

    val steamshipPrices: Map[City, List[Int]] = ListMap(
      Cormaya    -> List( 0 , 200, 160, inf),
      Farmine    -> List(210,  0 , 200, inf),
      Kazordoon  -> List( 60,  60,  0 ,  60),
      Thais      -> List(inf, inf,  60,  0 ),
    )

    val prices = List(boatPrices, magicCarpetPrices, steamshipPrices)


    def cheapestPath(start: City, end: City) = {
      val possibilities: Map[City, List[Leg]] = City.values.map { city =>
        val legs: List[Leg] = prices.flatMap { priceMap =>
          val cities = priceMap.keys.toList
          priceMap.get(city).map(_.zipWithIndex.collect {
            case (price, index) if price > 0 && price < inf => Leg(city, cities(index), price, Boat)
          }).toList.flatten
        }
        city -> legs
      }.toMap
      
      var cheapest = inf
      def go(start: City, end: City, price: Int = 0, legs: List[Leg] = List.empty[Leg]): Option[(Int, List[Leg])] = {
        if (start == end) {
          cheapest = price
          Some((price, legs))
        } else if (price > cheapest) {
          None
        } else {
          possibilities(start).collect {
            case leg @ Leg(_, newStart, legPrice, _) if legs.forall(_.start != newStart) =>
              go(newStart, end, price + legPrice, legs :+ leg)
          }.flatten.minByOption(_._1)
        }
      }
      go(start, end)
    }

    println(cheapestPath(Kazordoon, Ankrahmun))
  }
}
