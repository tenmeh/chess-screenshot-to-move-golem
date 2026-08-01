// Interactive analysis board: chessboard.js for rendering/drag-drop, chess.js
// for legality, and Shiny inputs/messages to sync with the server.
//
// Client-side chess.js is the same bundle the server runs in V8, so both sides
// agree on the rules. The server owns evaluation; the browser owns the move
// tree and only reports the resulting FEN.
//
// Every message carries explicit, already-namespaced element/input ids from the
// R module (via session$ns), so this file never reconstructs Shiny ids itself.
(function () {
  var boards = {}; // container id -> {board, game, stateInput, orientation}
  var pending = {}; // container id -> board-set message received before build

  function sendState(cid) {
    var st = boards[cid];
    if (!st) return;
    var g = st.game;
    var verbose = g.history({ verbose: true });
    var over = typeof g.isGameOver === "function" ? g.isGameOver() : g.game_over();
    Shiny.setInputValue(
      st.stateInput,
      {
        fen: g.fen(),
        turn: g.turn(),
        history: g.history(),
        last: verbose.length ? verbose[verbose.length - 1].san : null,
        ply: verbose.length,
        game_over: over,
        nonce: Math.random()
      },
      { priority: "event" }
    );
  }

  function onDragStart(cid) {
    return function (source, piece) {
      var g = boards[cid].game;
      var over = typeof g.isGameOver === "function" ? g.isGameOver() : g.game_over();
      if (over) return false;
      // Only the side to move may be dragged.
      if (piece.search(new RegExp("^" + (g.turn() === "w" ? "b" : "w"))) !== -1) {
        return false;
      }
    };
  }

  // Try a move, updating the board and telling the server if it was legal.
  // Returns true when the move was played.
  function tryMove(cid, from, to) {
    var st = boards[cid];
    var move = null;
    try {
      move = st.game.move({ from: from, to: to, promotion: "q" });
    } catch (e) {
      move = null; // chess.js 1.x throws on an illegal move
    }
    if (move === null) return false;
    sendState(cid);
    return true;
  }

  function onDrop(cid) {
    return function (source, target) {
      clearSelection(cid);
      // A drag is a completed interaction; the click that follows it must not
      // be read as the first tap of a tap-to-move.
      boards[cid].suppressClick = true;
      if (!tryMove(cid, source, target)) return "snapback";
    };
  }

  // ---- tap to move -------------------------------------------------------
  // Dragging a piece across a 34px square with a thumb is miserable, and on a
  // phone that is the only size a board comes in. Tapping the piece and then
  // its destination is the interaction every mobile chess app uses, and it
  // costs desktop users nothing - drag still works exactly as before.

  function clearSelection(cid) {
    var st = boards[cid];
    if (!st) return;
    var host = document.getElementById(cid);
    if (host) {
      host.querySelectorAll(".cv-sq-selected, .cv-sq-target").forEach(function (e) {
        e.classList.remove("cv-sq-selected", "cv-sq-target");
      });
    }
    st.selected = null;
  }

  function selectSquare(cid, square) {
    var st = boards[cid];
    var host = document.getElementById(cid);
    if (!st || !host) return;
    clearSelection(cid);
    st.selected = square;

    var from = host.querySelector('[data-square="' + square + '"]');
    if (from) from.classList.add("cv-sq-selected");

    // Show where it can actually go. chess.js is already here and authoritative,
    // so this cannot disagree with what a move attempt will accept.
    var moves = [];
    try {
      moves = st.game.moves({ square: square, verbose: true }) || [];
    } catch (e) {
      moves = [];
    }
    moves.forEach(function (m) {
      var el = host.querySelector('[data-square="' + m.to + '"]');
      if (el) el.classList.add("cv-sq-target");
    });
  }

  function ownPieceAt(cid, square) {
    var g = boards[cid].game;
    var p = null;
    try {
      p = g.get(square);
    } catch (e) {
      p = null;
    }
    return p && p.color === g.turn();
  }

  function onBoardClick(cid) {
    return function (ev) {
      var st = boards[cid];
      if (!st) return;
      // Swallow exactly one click after a drag finishes.
      if (st.suppressClick) {
        st.suppressClick = false;
        return;
      }
      var cell = ev.target.closest("[data-square]");
      if (!cell) return;
      var square = cell.getAttribute("data-square");

      var over =
        typeof st.game.isGameOver === "function"
          ? st.game.isGameOver()
          : st.game.game_over();
      if (over) return;

      if (!st.selected) {
        if (ownPieceAt(cid, square)) selectSquare(cid, square);
        return;
      }
      if (st.selected === square) {
        clearSelection(cid); // tapping it again puts it back down
        return;
      }
      if (tryMove(cid, st.selected, square)) {
        clearSelection(cid);
        st.board.position(st.game.fen());
        return;
      }
      // Not a legal destination: treat it as picking a different piece, or
      // as a miss.
      if (ownPieceAt(cid, square)) selectSquare(cid, square);
      else clearSelection(cid);
    };
  }

  function onSnapEnd(cid) {
    return function () {
      boards[cid].board.position(boards[cid].game.fen());
    };
  }

  function build(msg) {
    var cid = msg.container;
    var game;
    try {
      game = new window.ChessCtor(msg.fen || undefined);
    } catch (e) {
      // chess.js throws on a structurally illegal FEN (missing king, pawns on
      // the back rank, ...). The server is expected to filter these out
      // before sending a create/set message, but falling back to the start
      // position beats leaving this container's board half-built forever.
      game = new window.ChessCtor();
    }
    var board = window.Chessboard(cid, {
      draggable: true,
      position: game.fen(),
      orientation: msg.orientation || "white",
      // Full URL template from R, e.g. "pieces/{piece}.svg" - the extension
      // varies because a few sets ship webp/png instead of svg.
      pieceTheme: msg.pieceTheme,
      onDragStart: onDragStart(cid),
      onDrop: onDrop(cid),
      onSnapEnd: onSnapEnd(cid)
    });
    boards[cid] = {
      board: board,
      game: game,
      stateInput: msg.stateInput,
      orientation: msg.orientation || "white",
      selected: null,
      suppressClick: false
    };
    // Delegated, so it survives chessboard.js rebuilding the squares on every
    // position change.
    var host = document.getElementById(cid);
    if (host) host.addEventListener("click", onBoardClick(cid));
    window.addEventListener("resize", function () {
      board.resize();
      redrawArrows(cid);
    });
    sendState(cid);
  }

  // chessboard.js fixes its square size from the container width at build
  // time, so building while the tab is still hidden (width 0) produces a 0px
  // board that later resizes don't reliably repair. Wait for the container to
  // actually have width - i.e. the tab has been shown - then build once, and
  // keep re-fitting on later size changes.
  function create(msg) {
    var cid = msg.container;
    var host = document.getElementById(cid);
    if (!host) return;

    var built = false;
    var lastW = 0;

    function tryBuild() {
      var w = host.clientWidth;
      if (!built && w > 0) {
        built = true;
        lastW = w;
        build(msg);
        // Apply any position that arrived while we were waiting.
        if (pending[cid]) {
          applySet(pending[cid]);
          delete pending[cid];
        }
      } else if (built && w > 0 && Math.abs(w - lastW) > 1) {
        lastW = w;
        boards[cid].board.resize();
        redrawArrows(cid);
      }
    }

    if (typeof ResizeObserver === "function") {
      new ResizeObserver(tryBuild).observe(host);
    }
    // ResizeObserver does not always fire for a display:none -> visible tab
    // switch, so poll as well until the board exists.
    var ticks = 0;
    var timer = setInterval(function () {
      tryBuild();
      if (built || ticks++ > 600) clearInterval(timer);
    }, 100);
    tryBuild();
  }

  // ---- arrows ------------------------------------------------------------
  var lastArrows = {}; // container id -> arrow spec, kept for redraw on resize

  function redrawArrows(cid) {
    var spec = lastArrows[cid];
    if (spec) drawArrows(cid, spec);
  }

  function drawArrows(cid, arrows) {
    var st = boards[cid];
    var host = document.getElementById(cid);
    if (!st || !host) return;

    var old = host.querySelector(".cv-arrows");
    if (old) old.remove();
    lastArrows[cid] = arrows;
    if (!arrows || !arrows.length) return;

    var size = host.clientWidth;
    if (!size) return;
    var sq = size / 8;
    var flipped = st.board.orientation() === "black";
    var ns = "http://www.w3.org/2000/svg";
    var svg = document.createElementNS(ns, "svg");
    svg.setAttribute("class", "cv-arrows");
    svg.setAttribute("width", size);
    svg.setAttribute("height", size);
    var defs = document.createElementNS(ns, "defs");
    svg.appendChild(defs);

    function centre(square) {
      var file = square.charCodeAt(0) - 97;
      var rank = parseInt(square[1], 10) - 1;
      var x = flipped ? 7 - file : file;
      var y = flipped ? rank : 7 - rank;
      return { x: (x + 0.5) * sq, y: (y + 0.5) * sq };
    }

    arrows.forEach(function (a, i) {
      var mkId = "cv-head-" + cid.replace(/[^A-Za-z0-9_-]/g, "") + "-" + i;
      var mk = document.createElementNS(ns, "marker");
      mk.setAttribute("id", mkId);
      mk.setAttribute("markerWidth", "4");
      mk.setAttribute("markerHeight", "4");
      mk.setAttribute("refX", "2.6");
      mk.setAttribute("refY", "2");
      mk.setAttribute("orient", "auto");
      var tip = document.createElementNS(ns, "path");
      tip.setAttribute("d", "M0,0 L4,2 L0,4 z");
      tip.setAttribute("fill", a.color);
      mk.appendChild(tip);
      defs.appendChild(mk);

      var from = centre(a.from);
      var to = centre(a.to);
      var line = document.createElementNS(ns, "line");
      line.setAttribute("x1", from.x);
      line.setAttribute("y1", from.y);
      line.setAttribute("x2", to.x);
      line.setAttribute("y2", to.y);
      line.setAttribute("stroke", a.color);
      line.setAttribute("stroke-width", a.width || sq * 0.14);
      line.setAttribute("stroke-linecap", "round");
      line.setAttribute("opacity", a.opacity || 0.75);
      line.setAttribute("marker-end", "url(#" + mkId + ")");
      svg.appendChild(line);
    });

    host.appendChild(svg);
  }

  // ---- Shiny message handlers -------------------------------------------
  Shiny.addCustomMessageHandler("chessvision-board-create", function (msg) {
    // The container may not exist yet when the tab has never been shown.
    var tries = 0;
    (function attempt() {
      if (document.getElementById(msg.container)) {
        create(msg);
      } else if (tries++ < 60) {
        setTimeout(attempt, 50);
      }
    })();
  });

  function applySet(msg) {
    var st = boards[msg.container];
    if (!st) {
      // Board not built yet (its tab has never been opened); remember the
      // position and apply it once the board exists.
      pending[msg.container] = msg;
      return;
    }
    var game;
    try {
      game = new window.ChessCtor(msg.fen);
    } catch (e) {
      // A structurally illegal FEN (see build()); ignore rather than break
      // an otherwise-working board.
      return;
    }
    st.game = game;
    // The selected piece may not exist in the new position, and the highlight
    // would survive onto whatever now sits on that square.
    clearSelection(msg.container);
    st.board.position(msg.fen, false);
    if (msg.orientation && msg.orientation !== st.orientation) {
      st.orientation = msg.orientation;
      st.board.orientation(msg.orientation);
    }

    // A position just arrived from somewhere else on the page. On a phone the
    // board can be two screens further down, so bring it into view - but only
    // when it is actually off screen, which means this does nothing on a
    // desktop where the board was already in front of you.
    var host = document.getElementById(msg.container);
    if (host && typeof host.scrollIntoView === "function") {
      var r = host.getBoundingClientRect();
      var vh = window.innerHeight || document.documentElement.clientHeight;
      var visible = r.top < vh * 0.75 && r.bottom > vh * 0.25;
      if (!visible) {
        host.scrollIntoView({ behavior: "smooth", block: "center" });
      }
    }

    sendState(msg.container);
  }

  Shiny.addCustomMessageHandler("chessvision-board-set", applySet);

  Shiny.addCustomMessageHandler("chessvision-board-undo", function (msg) {
    var st = boards[msg.container];
    if (!st) return;
    st.game.undo();
    st.board.position(st.game.fen());
    sendState(msg.container);
  });

  Shiny.addCustomMessageHandler("chessvision-board-flip", function (msg) {
    var st = boards[msg.container];
    if (!st) return;
    st.board.flip();
    st.orientation = st.board.orientation();
    redrawArrows(msg.container);
  });

  // Accepts one move or several. The live tracker sends several when it had to
  // infer a ply it never saw on screen; playing them in turn keeps this board's
  // own history complete, which a jump to the final position would not.
  Shiny.addCustomMessageHandler("chessvision-board-move", function (msg) {
    var st = boards[msg.container];
    if (!st) return;
    var list = msg.ucis || msg.uci;
    if (!Array.isArray(list)) list = [list];
    var played = 0;
    list.forEach(function (uci) {
      if (!uci) return;
      var mv = { from: uci.slice(0, 2), to: uci.slice(2, 4) };
      if (uci.length > 4) mv.promotion = uci[4];
      try {
        st.game.move(mv);
        played++;
      } catch (e) {
        /* ignore an illegal suggestion */
      }
    });
    if (!played) return;
    st.board.position(st.game.fen());
    sendState(msg.container);
  });

  Shiny.addCustomMessageHandler("chessvision-board-arrows", function (msg) {
    drawArrows(msg.container, msg.arrows);
  });

  Shiny.addCustomMessageHandler("chessvision-evalbar", function (msg) {
    var w = document.getElementById(msg.white_id);
    var b = document.getElementById(msg.black_id);
    if (!w || !b) return;
    var pct = Math.max(0, Math.min(100, msg.pct));
    w.style.height = pct + "%";
    b.style.height = 100 - pct + "%";
  });
})();
