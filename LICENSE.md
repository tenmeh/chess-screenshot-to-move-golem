# GNU Affero General Public License v3.0

Tanmai Chess - read a chess board from a screenshot and find the move
Copyright (C) 2026 Tanmay Chanda

This program is free software: you can redistribute it and/or modify it under
the terms of the GNU Affero General Public License as published by the Free
Software Foundation, either version 3 of the License, or (at your option) any
later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public License along
with this program. If not, see <https://www.gnu.org/licenses/>.

The complete licence text is in [LICENSE](LICENSE).

## What this means in practice

You may use, study, modify and share this software freely. If you distribute it,
or a modified version of it, the source must be available under this same
licence.

The "Affero" part matters for this project specifically: **running a modified
version as a network service counts as distribution.** Anyone who hosts their
own copy of this app, changed in any way, has to make their source available to
the people using it. An ordinary GPL would let them keep those changes private
as long as they never shipped the code, which for a web app is the whole of the
loophole.

## Third-party components

This repository bundles or downloads work by other people, under their own
terms:

| Component | Licence |
| --- | --- |
| [chess.js](https://github.com/jhlywa/chess.js) (`inst/js/`) | MIT |
| [chessboard.js](https://chessboardjs.com) (`inst/app/www/`) | MIT |
| cburnett piece SVGs (`inst/svg/`, via python-chess) | GPL-3.0 |
| [Poppins](https://fonts.google.com/specimen/Poppins) (`pkgdown/fonts/`) | SIL Open Font License 1.1 |
| [Stockfish](https://stockfishchess.org) (installed at build time) | GPL-3.0 |
| [lc0](https://github.com/LeelaChessZero/lc0) (built at image build time) | GPL-3.0 |
| [Maia](https://github.com/CSSLab/maia-chess) weights | GPL-3.0 |

Stockfish and lc0 are run as separate processes over UCI rather than linked
into this program. Piece art for other sets is fetched at runtime for personal
use and is not redistributed here; `piece_set_manifest()` records each set's
licence.

## Earlier versions

Releases up to and including v1.0.3 were published under the MIT licence. That
cannot be withdrawn - anyone who obtained those versions keeps their MIT rights
to them. This licence applies from v1.1.0 onward.
