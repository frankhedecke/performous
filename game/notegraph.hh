#pragma once

#include "animvalue.hh"
#include "surface.hh"
#include "notes.hh"
#include "player.hh"

class Song;
class Database;

/// handles drawing of notes and waves
class NoteGraph {
  public:
	enum Position {FULLSCREEN, TOP, BOTTOM, LEFT, RIGHT, BETA_TOP, BETA_BOTTOM};
	/// constructor
	NoteGraph(VocalTrack const& vocal);
	/// resets NoteGraph and Notes
	void reset();
	/** draws NoteGraph (notelines, notes, waves)
	 * @param time at which time to draw
	 * @param players reference to the list of singing Players
	 */
	void draw(double time, Database const& database, Position position = NoteGraph::FULLSCREEN);
  private:
	/// draw notebars
	void drawNotes(std::_List_const_iterator<Player> player_it);
	/// draw waves (what players are singing)
	void drawWaves(std::_List_const_iterator<Player> player_it);
	VocalTrack const& m_vocal;
	Texture m_wave;
	Texture m_star;
	Texture m_star_hl;
	Texture m_notebar_std;
	Texture m_notebar_std_gold;
	Texture m_notebar_blue;
	Texture m_notebar_blue_gold;
	Texture m_notebar_red;
	Texture m_notebar_red_gold;
	Texture m_notebar_green;
	Texture m_notebar_green_gold;
	Texture m_notebar_yellow;
	Texture m_notebar_yellow_gold;
	Texture m_notebar_fuchsia;
	Texture m_notebar_fuchsia_gold;
	Texture m_notebar_lightgreen;
	Texture m_notebar_lightgreen_gold;
	Texture m_notebar_purple;
	Texture m_notebar_purple_gold;
	Texture m_notebar_aqua;
	Texture m_notebar_aqua_gold;
	Texture m_notebar_hl;
	Texture m_notebarfs;
	Texture m_notebarfs_hl;
	float m_notealpha;
	AnimValue m_nlTop, m_nlBottom;
	Notes::const_iterator m_songit;
	double m_time;
	double m_max, m_min, m_noteUnit, m_baseY, m_baseX;
	int m_cur_player;
	int m_cur_offset;
	int m_max_players;
};

