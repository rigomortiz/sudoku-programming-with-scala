package com.rigobertocanseco.sudoku.game

import org.gillius.jalleg.binding.ALLEGRO_COLOR
import org.gillius.jalleg.binding.AllegroLibrary.{ALLEGRO_ALIGN_CENTER, ALLEGRO_KEY_SPACE, al_clear_to_color, al_create_builtin_font, al_draw_line, al_draw_text, al_map_rgb, al_map_rgb_f}
import org.gillius.jalleg.framework.{AllegroAddon, Game}
import org.gillius.jalleg.binding.ALLEGRO_FONT

class SudokuGame extends Game {
  private val white: ALLEGRO_COLOR = al_map_rgb_f(1.toByte, 1.toByte, 1.toByte)
  private var font: ALLEGRO_FONT = _

  override def onAllegroStarted(): Unit = {
    initAddons(AllegroAddon.Primitives, AllegroAddon.Font, AllegroAddon.Keyboard, AllegroAddon.Joystick,
      AllegroAddon.Mouse, AllegroAddon.Audio, AllegroAddon.Haptic);
    font = al_create_builtin_font()
  }

  protected def render(): Unit = {
    //al_clear_to_color(al_map_rgb(0, 0, 0))
    //al_draw_line(50f, 0f, 50f, 100f, white, 1f)
    al_draw_text(font, white, 100f, 10f,  ALLEGRO_ALIGN_CENTER, "Hello World")
  }

  protected def update(): Unit = {
    if (isKeyDown(ALLEGRO_KEY_SPACE)) {
      stop()
    }
  }
}