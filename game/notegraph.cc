#include "notegraph.hh"

#include "configuration.hh"
#include "database.hh"
#include "engine.hh"
#include "player.hh"

Dimensions dimensions; // Make a public member variable

NoteGraph::NoteGraph(VocalTrack const& vocal):
	m_vocal(vocal),
	m_wave(findFile("wave.png")),
	m_star(findFile("star.svg")), m_star_hl(findFile("star_glow.svg")),
	m_notebar_std(findFile("notebar_std.svg")), m_notebar_std_gold(findFile("notebar_std_gold.svg")),
	m_notebar_blue(findFile("notebar_blue.svg")), m_notebar_blue_gold(findFile("notebar_blue_gold.svg")),
	m_notebar_red(findFile("notebar_red.svg")), m_notebar_red_gold(findFile("notebar_red_gold.svg")), 
	m_notebar_green(findFile("notebar_green.svg")), m_notebar_green_gold(findFile("notebar_green_gold.svg")), 
	m_notebar_yellow(findFile("notebar_yellow.svg")), m_notebar_yellow_gold(findFile("notebar_yellow_gold.svg")),
	m_notebar_fuchsia(findFile("notebar_fuchsia.svg")), m_notebar_fuchsia_gold(findFile("notebar_fuchsia_gold.svg")),
	m_notebar_lightgreen(findFile("notebar_lightgreen.svg")), m_notebar_lightgreen_gold(findFile("notebar_lightgreen_gold.svg")),
	m_notebar_purple(findFile("notebar_purple.svg")), m_notebar_purple_gold(findFile("notebar_purple_gold.svg")),
	m_notebar_aqua(findFile("notebar_aqua.svg")), m_notebar_aqua_gold(findFile("notebar_aqua_gold.svg")),
	m_notebar_hl(findFile("notebar_hi.svg")),
	m_notebarfs(findFile("notebarfs.svg")), m_notebarfs_hl(findFile("notebarfs-hl.png")),
	m_notealpha(0.0f), m_nlTop(0.0, 4.0), m_nlBottom(0.0, 4.0), m_time()
{
	dimensions.stretch(1.0, 0.5); // Initial dimensions, probably overridden from somewhere
	m_nlTop.setTarget(m_vocal.noteMax, true);
	m_nlBottom.setTarget(m_vocal.noteMin, true);
	for (auto const& n: m_vocal.notes) n.stars.clear(); // Reset stars
	reset();
}

void NoteGraph::reset() {
	m_vocalit_beg = m_vocal.notes.begin();
	m_vocalit_end = m_vocal.notes.begin();
	Notes::const_iterator eof = m_vocal.notes.end();

	// iterate for the first time to next sleep note
	m_cur_lowest = m_vocalit_beg->note;
	m_cur_highest = m_vocalit_beg->note;
	for (Notes::const_iterator it = m_vocalit_beg; it != eof && it->type != Note::SLEEP; ++it) {
		m_vocalit_end = it;
		if(it->note > m_cur_highest) m_cur_highest = it->note;
		if(it->note < m_cur_lowest) m_cur_lowest = it->note;
	}

	m_songit = m_vocal.notes.begin();
}

namespace {
	void drawNotebar(Texture const& texture, double x, double ybeg, double yend, double w, double h) {
		glutil::VertexArray va;
		UseTexture tblock(texture);

		// The front cap begins
		va.texCoord(0.0f, 0.0f).vertex(x, ybeg);
		va.texCoord(0.0f, 1.0f).vertex(x, ybeg + h);
		if (w >= 2.0 * h) {
			// Calculate the y coordinates of the middle part
			double tmp = h / w;  // h = cap size (because it is a h by h square)
			double y1 = (1.0 - tmp) * ybeg + tmp * yend;
			double y2 = tmp * ybeg + (1.0 - tmp) * yend;
			// The middle part between caps
			va.texCoord(0.5f, 0.0f).vertex(x + h, y1);
			va.texCoord(0.5f, 1.0f).vertex(x + h, y1 + h);
			va.texCoord(0.5f, 0.0f).vertex(x + w - h, y2);
			va.texCoord(0.5f, 1.0f).vertex(x + w - h, y2 + h);
		} else {
			// Note is too short to even fit caps, crop to fit.
			double ymid = 0.5 * (ybeg + yend);
			float crop = 0.25f * w / h;
			va.texCoord(crop, 0.0f).vertex(x + 0.5 * w, ymid);
			va.texCoord(crop, 1.0f).vertex(x + 0.5 * w, ymid + h);
			va.texCoord(1.0f - crop, 0.0f).vertex(x + 0.5 * w, ymid);
			va.texCoord(1.0f - crop, 1.0f).vertex(x + 0.5 * w, ymid + h);
		}
		// The rear cap ends
		va.texCoord(1.0f, 0.0f).vertex(x + w, yend);
		va.texCoord(1.0f, 1.0f).vertex(x + w, yend + h);

		va.draw();
	}
}

const double baseLine = -0.2;
const double pixUnit = 0.2;

void NoteGraph::draw(double time, Database const& database, Position position) {
	if (time < m_time) reset();
	m_time = time;
	// Update m_songit (which note to start the rendering from)
	while (m_songit != m_vocal.notes.end() && (m_songit->type == Note::SLEEP || m_songit->end < time - (baseLine + 0.5) / pixUnit)) ++m_songit;

	// Automatically zooming notelines
	{
		int low = m_vocal.noteMax;
		int high = m_vocal.noteMin;
		int low2 = m_vocal.noteMax;
		int high2 = m_vocal.noteMin;
		for (auto it = m_songit; it != m_vocal.notes.end() && it->begin < time + 15.0; ++it) {
			if (it->type == Note::SLEEP) continue;
			if (it->note < low) low = it->note;
			if (it->note > high) high = it->note;
			if (it->begin > time + 8.0) continue;
			if (it->note < low2) low2 = it->note;
			if (it->note > high2) high2 = it->note;
		}
		if (low2 <= high2) {
			m_nlTop.setRange(high2, high);
			m_nlBottom.setRange(low, low2);
		}
	}
	// TODO check if non BETA positions still work
	switch(position) {
		case NoteGraph::FULLSCREEN:
			dimensions.stretch(1.0, 0.50).center();
			break;
		case NoteGraph::TOP:
			dimensions.stretch(1.0, 0.32).bottom(0.0);
			break;
		// why BOTTOM and top()?
		case NoteGraph::BOTTOM:
			dimensions.stretch(1.0, 0.32).top(0.0);
			break;
		case NoteGraph::LEFT:
			dimensions.stretch(0.50, 0.50).center().left(-0.5);
			break;
		case NoteGraph::RIGHT:
			dimensions.stretch(0.50, 0.50).center().right();
			break;
		case NoteGraph::BETA_TOP:
			dimensions.stretch(1.0, 0.25).bottom(0.0);
			break;
		case NoteGraph::BETA_BOTTOM:
			dimensions.stretch(1.0, 0.25).top(0.0);
			break;
	}
	m_max = m_nlTop.get() + 7.0;
	m_min = m_nlBottom.get() - 7.0;
	m_noteUnit = -dimensions.h() / std::max(48.0 * dimensions.h(), m_max - m_min);
	m_baseY = -0.5 * (m_min + m_max) * m_noteUnit + dimensions.yc();
	m_baseX = baseLine - m_time * pixUnit + dimensions.xc();  // FIXME: Moving in X direction requires additional love (is b0rked now, keep it centered at zero)
	m_max_players = database.cur.size();
	m_cur_player = 0;
	m_cur_offset = 0;
	if (position == NoteGraph::BETA_BOTTOM)
		m_cur_offset = (m_max_players + 1) / 2;

	// Fading notelines handing
	if (m_songit == m_vocal.notes.end() || m_songit->begin > m_time + 3.0) m_notealpha -= 0.02f;
	else if (m_notealpha < 1.0f) m_notealpha += 0.02f;
	if (m_notealpha <= 0.0f) { m_notealpha = 0.0f; return; }

	ColorTrans c(Color::alpha(m_notealpha));

	for (auto player_it = database.cur.begin(); player_it != database.cur.end(); ++player_it) {

		// filter players for some views
		// TODO players should be filtered elsewhere

		bool player_select = false;
		switch(position) {
			case NoteGraph::BETA_TOP:
				if (m_cur_player <= ((m_max_players - 1) / 2)) {
					player_select = true;
				}
				break;
			case NoteGraph::BETA_BOTTOM:
				if (m_cur_player > ((m_max_players - 1) / 2)) {
					player_select = true;
				}
				break;
			default:
				player_select = true;
				break;
		}

		if (player_select) {
			drawNotes(player_it);
			drawStars(player_it, position);
			if (config["game/pitch"].b())
				drawWaves(player_it);
		}
		++m_cur_player;
	}
}

void NoteGraph::draw_new(double time, VocalTrack& vocal, Database const& database, Position position) {

	// TODO check if BETA_BOTTOM works
	// support stretching but not moving
	switch(position) {
		case NoteGraph::FULLSCREEN:
		case NoteGraph::TOP:
		case NoteGraph::BOTTOM:
		case NoteGraph::LEFT:
		case NoteGraph::RIGHT:
		case NoteGraph::BETA_TOP:
			dimensions.stretch(0.9, 0.25).bottom(-0.05);
			break;
		case NoteGraph::BETA_BOTTOM:
			dimensions.stretch(1.0, 0.25).top(0.0);
			break;
	}

	Notes::const_iterator eof = vocal.notes.end();

	if(time > m_vocalit_end->end) {
		// and check eof
		m_vocalit_beg = m_vocalit_end;
		m_vocalit_beg++;
		if (m_vocalit_beg != eof)
			m_vocalit_beg++;
		// iterate to next sleep note
		m_cur_lowest = m_vocalit_beg->note;
		m_cur_highest = m_vocalit_beg->note;
		for (Notes::const_iterator it = m_vocalit_beg; it != eof && it->type != Note::SLEEP; ++it) {
			m_vocalit_end = it;
			if(it->note > m_cur_highest) m_cur_highest = it->note;
			if(it->note < m_cur_lowest) m_cur_lowest = it->note;
		}
	}

	// start drawing 2s before first note
	if (m_vocalit_beg->begin - time < 2) {

		Texture* texture = &m_notebar_red;
		double time_span = m_vocalit_end->end - m_vocalit_beg->begin;
		int noterange = m_cur_highest - m_cur_lowest + 1;
		if (noterange == 0) noterange = 1;

		for (auto it = m_vocalit_beg; it != m_vocal.notes.end() && it <= m_vocalit_end; ++it) {
			if (it->type == Note::SLEEP) continue;

			// TODO calculate high depending on noterange;
			float h = 0.03;
			float w = (it->end - it->begin) / time_span * dimensions.w() + h; // borders left + right == height

			// x - (left border == height / 2)
			float x = dimensions.x1() - h / 2;
			x += (it->begin - m_vocalit_beg->begin) / time_span * dimensions.w();

			float y = dimensions.y2();
			if (noterange != 1)
				y -= dimensions.h() * (it->note - m_cur_lowest) / noterange;
			else
				y += dimensions.h() / 2;

			drawNotebar(*texture, x , y, y, w, h);
		}
	}
}

void NoteGraph::drawNotes(std::_List_const_iterator<Player> player_it) {

	// calc displayed players
	int player = m_cur_player - m_cur_offset;
	int max_players = 0;
	if (m_cur_offset == 0) { // TOP
		max_players = m_max_players - m_max_players / 2;
	} else if ((m_max_players % 2) == 0) { // BOTTOM
		max_players = m_max_players - m_max_players / 2;
	} else if ((m_max_players % 2) == 1) { // BOTTOM
		max_players = m_max_players / 2;
	}

	// Draw notes
	if ((0 <= player) && (player < 4)) {
		for (auto it = m_songit; it != m_vocal.notes.end() && it->begin < m_time - (baseLine - 0.5) / pixUnit; ++it) {
			if (it->type == Note::SLEEP) continue;

			// TODO use c++11 array OR boost::ptr_array
			Texture* textures[max_players];
			for (int i = 0; i < max_players; ++i) {
				textures[i] = &m_notebar_std;
			}
			Texture* t_note_hl =  &m_notebar_hl;

			switch (it->type) {
				case Note::NORMAL: case Note::SLIDE: {
					Color col = player_it->m_color;

					// guess the color
					if (col.r > 0.5) { // red + yellow + fuchsia + purple
						if (col.b > 0.5) { // fuchsia + purple
							if (col.g > 0.3) textures[player] = &m_notebar_fuchsia;
							else textures[player] = &m_notebar_purple;
						} else { // red + yellow
							if (col.g > 0.9) textures[player] = &m_notebar_yellow;
							else textures[player] = &m_notebar_red;
						}
					} else { // blue + green + lightgreen + aqua
						if (col.b > 0.5) { // blue + aqua
							if (col.g > 0.9) textures[player] = &m_notebar_aqua;
							else textures[player] = &m_notebar_blue;
						} else { // green + lightgreen
							if (col.b > 0.2) textures[player] = &m_notebar_lightgreen;
							else textures[player] = &m_notebar_green;
						}
					}
					break;
				}
				case Note::GOLDEN: {
					Color col = player_it->m_color;

					// guess the color
					if (col.r > 0.5) { // red + yellow + fuchsia + purple
						if (col.b > 0.5) { // fuchsia + purple
							if (col.g > 0.3) textures[player] = &m_notebar_fuchsia_gold;
							else textures[player] = &m_notebar_purple_gold;
						} else { // red + yellow
							if (col.g > 0.9) textures[player] = &m_notebar_yellow_gold;
							else textures[player] = &m_notebar_red_gold;
						}
					} else { // blue + green + lightgreen + aqua
						if (col.b > 0.5) { // blue + aqua
							if (col.g > 0.9) textures[player] = &m_notebar_aqua_gold;
							else textures[player] = &m_notebar_blue_gold;
						} else { // green + lightgreen
							if (col.b > 0.2) textures[player] = &m_notebar_lightgreen_gold;
							else textures[player] = &m_notebar_green_gold;
						}
					}
					break;
				}
				case Note::FREESTYLE: // Freestyle notes use custom handling
				{
					double alpha = it->power;
					Dimensions dim;
					dim.middle(m_baseX + 0.5 * (it->begin + it->end) * pixUnit).center(m_baseY + it->note * m_noteUnit).stretch(
							(it->end - it->begin) * pixUnit, -m_noteUnit * 12.0);
					float xoffset = 0.1 * m_time / m_notebarfs.dimensions.ar();
					m_notebarfs.draw(dim, TexCoords(xoffset, 0.0, xoffset + dim.ar() / m_notebarfs.dimensions.ar(), 1.0));
					if (alpha > 0.0) {
						float xoffset = rand() / double(RAND_MAX);
						m_notebarfs_hl.draw(dim, TexCoords(xoffset, 0.0, xoffset + dim.ar() / m_notebarfs_hl.dimensions.ar(), 1.0));
					}
				}
				continue;
			  default: throw std::logic_error("Unknown note type: don't know how to render");
			}

			// TODO change according to max_players
			double x = m_baseX + it->begin * pixUnit + m_noteUnit; // left x coordinate: begin minus border (side borders -noteUnit wide)
			// double ybeg = m_baseY - 0.08 + (it->notePrev + 1) * m_noteUnit + player * 0.08; // top y coordinate (on the one higher note line)
			// TODO consider max players and center y_offset
			double ybeg = m_baseY + (it->notePrev + 1) * m_noteUnit + player * 0.08; // top y coordinate (on the one higher note line)
			double yend = m_baseY + (it->note + 1) * m_noteUnit + player * 0.08; // top y coordinate (on the one higher note line)
			double w = (it->end - it->begin) * pixUnit - m_noteUnit * 2.0; // width: including borders on both sides
			double h = -m_noteUnit * 2.0; // height: 0.5 border + 1.0 bar + 0.5 border = 2.0
			double alpha1 = it->power;
			double alpha2 = player_it->m_power;

			// draw notes
			drawNotebar(*textures[player], x, ybeg, yend, w, h);
			if (alpha2 > 0.0 && alpha1 > 0.0) {
				ColorTrans c(Color::alpha(alpha2));
				// draw highlights
				drawNotebar(*t_note_hl, x, ybeg, yend, w, h);
			}
		}
	}
}

namespace {
	void strip(glutil::VertexArray& va) {
		if (va.size() > 3) va.draw();
		va.clear();
	}
}

void NoteGraph::drawStars(std::_List_const_iterator<Player> player_it, Position position) {
	for (auto it = m_songit; it != m_vocal.notes.end() && it->begin < m_time - (baseLine - 0.5) / pixUnit; ++it) {
		for (std::vector<Color>::const_iterator it_col = it->stars.begin(); it_col != it->stars.end(); ++it_col) {
			Color col = player_it->m_color;
			if(col.r == it_col->r && col.g == it_col->g && col.b == it_col->b) {
				double x = m_baseX + it->begin * pixUnit + m_noteUnit; // left x coordinate: begin minus border (side borders -noteUnit wide)
				double w = (it->end - it->begin) * pixUnit - m_noteUnit * 2.0; // width: including borders on both sides
				float hh = -m_noteUnit;
				float centery = m_baseY + (it->note + 0.4) * m_noteUnit; // Star is 0.4 notes higher than current note
				//centery = centery - 0.08 + 0.08 * m_cur_player;
				// TODO consider max players and center y_offset
				int player = m_cur_player - m_cur_offset;
				centery += 0.08 * player;
				float centerx = x + w - 1.2 * hh; // Star is 1.2 units from end
				float rot = fmod(m_time * 5.0, 2.0 * M_PI); // They rotate!
				// TODO maybe evaluate smallerNoteGraph elsewhere
				bool smallerNoteGraph = ((position == NoteGraph::TOP) || (position == NoteGraph::BOTTOM) ||(position == NoteGraph::BETA_TOP) || (position == NoteGraph::BETA_BOTTOM));
				float zoom = (std::abs((rot-180) / 360.0f) * 0.8f + 0.6f) * (smallerNoteGraph ? 2.3 : 2.0) * hh;
				using namespace glmath;
				Transform trans(translate(vec3(centerx, centery, 0.0f)) * rotate(rot, vec3(0.0f, 0.0f, 1.0f)));
				{
					ColorTrans c(Color(col.r, col.g, col.b, col.a));
					m_star_hl.draw(Dimensions().stretch(zoom*1.2, zoom*1.2).center().middle(), TexCoords());
				}
				m_star.draw(Dimensions().stretch(zoom, zoom).center().middle(), TexCoords());
			}
		}
	}
}

// TODO consider max players and center y_offset
void NoteGraph::drawWaves(std::_List_const_iterator<Player> player_it) {
	int player = m_cur_player - m_cur_offset;
	if (m_vocal.notes.empty()) return; // Cannot draw without notes
	UseTexture tblock(m_wave);
	// TODO explain
	if (player_it->m_vocal.name != m_vocal.name)
		return;
	float const texOffset = 2.0 * m_time; // Offset for animating the wave texture
	Player::pitch_t const& pitch = player_it->m_pitch;
	size_t const beginIdx = std::max(0.0, m_time - 0.5 / pixUnit) / Engine::TIMESTEP; // At which pitch idx to start displaying the wave
	size_t const endIdx = player_it->m_pos;
	size_t idx = beginIdx;
	// Go back until silence (NaN freq) to allow proper wave phase to be calculated
	if (beginIdx < endIdx) while (idx > 0 && pitch[idx].first == pitch[idx].first) --idx;
	// Start processing
	float tex = texOffset;
	double t = idx * Engine::TIMESTEP;
	double oldval = getNaN();
	glutil::VertexArray va;
	auto noteIt = m_vocal.notes.begin();
	glmath::vec4 c(player_it->m_color.r, player_it->m_color.g, player_it->m_color.b, 1.0);
	for (; idx < endIdx; ++idx, t += Engine::TIMESTEP) {
		double const freq = pitch[idx].first;
		// If freq is NaN, we have nothing to process
		if (freq != freq) { oldval = getNaN(); tex = texOffset; continue; }
		tex += freq * 0.001; // Wave phase (texture coordinate)
		if (idx < beginIdx) continue; // Skip graphics rendering if out of screen
		double x = -0.2 + (t - m_time) * pixUnit;
		// Find the currently active note(s)
		while (noteIt != m_vocal.notes.end() && (noteIt->type == Note::SLEEP || t > noteIt->end)) ++noteIt;
		auto notePrev = noteIt;
		while (notePrev != m_vocal.notes.begin() && (notePrev->type == Note::SLEEP || t < notePrev->begin)) --notePrev;
		bool hasNote = (noteIt != m_vocal.notes.end());
		bool hasPrev = notePrev->type != Note::SLEEP && t >= notePrev->begin;
		double val;
		if (hasNote && hasPrev) val = 0.5 * (noteIt->note + notePrev->note);
		else if (hasNote) val = noteIt->note;
		else val = notePrev->note;
		// Now val contains the active note value. The following calculates note value for current freq:
		val += Note::diff(val, MusicalScale(m_vocal.scale).setFreq(freq).getNote());
		// Graphics positioning & animation:
		double y = m_baseY + 0.08 * player + val * m_noteUnit;
		double thickness = clamp(1.0 + pitch[idx].second / 60.0) + 0.5;
		thickness *= 1.0 + 0.2 * std::sin(tex - 2.0 * texOffset); // Further animation :)
		thickness *= -m_noteUnit;
		// If there has been a break or if the pitch change is too fast, terminate and begin a new one
		if (oldval != oldval || std::abs(oldval - val) > 1) strip(va);
		// Add a point or a pair of points
		if (!va.size()) va.texCoord(tex, 0.5f).color(c).vertex(x, y);
		else {
			va.texCoord(tex, 0.0f).color(c).vertex(x, y - thickness);
			va.texCoord(tex, 1.0f).color(c).vertex(x, y + thickness);
		}
		oldval = val;
	}
	strip(va);
}
