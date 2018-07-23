package com.ojha.red

class CatState {

  type State[S, +A] = S => (A, S)

  case class State

}
