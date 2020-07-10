package me.amuxix.travelsearch

import scala.collection.{MapView, mutable}
import me.amuxix.travelsearch.City._
import me.amuxix.travelsearch.Transport._
import me.amuxix.travelsearch.City

@main def travelSearch: Unit =
  val inf = 1000000

  val boatPrices: Map[City, List[Int]] = Map(
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
  
  val magicCarpetPrices: Map[City, List[Int]] = Map(
    //Darashia, Edron, Farmine, Femor Hills, Svargrond, Kazordoon, Issavi
    Darashia   -> List( 0 ,  40,  60,  60,  60,  80, 100),
    Edron      -> List( 40,  0 ,  60,  60,  60,  70, 100),
    Farmine    -> List( 60,  60,  0 ,  60,  60,  70, 100),
    FemorHills -> List( 60,  60,  60,  0 ,  60,  70, 100),
    Svargrond  -> List( 60,  60,  60,  60,  0 ,  70, 100),
    Kazordoon  -> List( 40,  40,  60,  60,  60,  0 , 100),
    Issavi     -> List( 40,  60,  60,  60,  60,  70,  0 ),
  )


  def cheapestPath(start: City, end: City) =
    val boatCities = boatPrices.keys.toList
    val carpetCities = magicCarpetPrices.keys.toList
    val possibilities: Map[City, List[Leg]] = City.entries.map { city =>
      val byBoat: Option[List[Leg]] = boatPrices.get(city).map(_.zipWithIndex.map {
        (price, index) => Leg(city, boatCities(index), price, Boat)
      })
      val byCarpet: Option[List[Leg]] = magicCarpetPrices.get(city).map(_.zipWithIndex.map {
        (price, index) => Leg(city, carpetCities(index), price, MagicCarpet)
      })
      city -> (byBoat.toList.flatten ++ byCarpet.toList.flatten)
    }.toMap

    var cheapest = inf
    var cheapestLegs = List.empty[Leg]
    def go(start: City, end: City, price: Int = 0, legs: List[Leg] = List.empty[Leg]): (Int, List[Leg]) =
      if (start == end)
        (price, legs)
      else
        possibilities(start).collect {
          case leg @ Leg(_, newStart, legPrice, _) if price <= cheapest => go(newStart, end, price + legPrice, legs :+ leg)
        }.minBy(_._1)
    go(start, end)

  println(cheapestPath(Thais, Kazordoon))


  /*def allPairsShortestPath(boatPrices: Map[City, List[Int]]): Map[City, Map[City, Int]] =
    val pricesMap: mutable.Map[City, mutable.Map[City, Int]] = boatPrices.view.mapValues { prices =>
      prices.zipWithIndex.map {
        (price, index) => City.entries(index) -> price
      }.toMap.to(mutable.Map)
    }.toMap.to(mutable.Map)

    var updated = true
    while (updated)
      updated = false
      for
        k <- City.entries
        i <- City.entries
        j <- City.entries
      yield
        val i1 = pricesMap(i)(k) + pricesMap(k)(j)
        if (pricesMap(i)(j) > i1)
          //pricesMap.update(i, pricesMap(i).update(j, i1))
          pricesMap(i).update(j, i1)
          updated =  true

    pricesMap.view.mapValues(_.to(Map)).toMap

  allPairsShortestPath(boatPrices)*/
