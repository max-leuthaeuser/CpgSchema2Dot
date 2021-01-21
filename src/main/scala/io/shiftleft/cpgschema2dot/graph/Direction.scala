package io.shiftleft.cpgschema2dot.graph

sealed trait Direction

case object In  extends Direction
case object Out extends Direction
