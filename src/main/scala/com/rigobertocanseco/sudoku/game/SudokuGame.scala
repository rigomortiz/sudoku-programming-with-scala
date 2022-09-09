package com.rigobertocanseco.sudoku.game

import org.gillius.jalleg.binding.ALLEGRO_COLOR
import org.gillius.jalleg.binding.AllegroLibrary.{ALLEGRO_KEY_SPACE, al_clear_to_color, al_draw_line, al_map_rgb}
import org.gillius.jalleg.framework.{AllegroAddon, Game}

class SudokuGame extends Game {
  private val white: ALLEGRO_COLOR = al_map_rgb(255.toByte, 255.toByte, 255.toByte)
  private val font = null
  override def onAllegroStarted(): Unit = {
    initAddons(AllegroAddon.Primitives, AllegroAddon.Font, AllegroAddon.Keyboard)
  }

  protected def render(): Unit = {
    al_clear_to_color(al_map_rgb(0, 0, 0))
    al_draw_line(50f, 0f, 50f, 100f, white, 1f)
  }

  protected def update(): Unit = {
    if (isKeyDown(ALLEGRO_KEY_SPACE)) {
      stop()
    }
  }
}