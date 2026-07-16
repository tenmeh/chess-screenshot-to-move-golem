// Clipboard-paste support for chess board screenshots.
//
// A ".paste-zone" element must have tabindex="0" and a data-target attribute
// holding the (fully namespaced) Shiny input id to populate. Click the zone
// to focus it, then Ctrl+V - the pasted image is sent to Shiny as a data URL.
document.addEventListener("paste", function (e) {
  var active = document.activeElement;
  var zone = active && active.closest ? active.closest(".paste-zone") : null;
  if (!zone) return;

  var items = (e.clipboardData || window.clipboardData).items;
  if (!items) return;

  for (var i = 0; i < items.length; i++) {
    if (items[i].type.indexOf("image") !== -1) {
      var file = items[i].getAsFile();
      var reader = new FileReader();
      reader.onload = function (evt) {
        Shiny.setInputValue(zone.dataset.target, evt.target.result, { priority: "event" });
      };
      reader.readAsDataURL(file);
      e.preventDefault();
      break;
    }
  }
});

document.addEventListener("DOMContentLoaded", function () {
  document.body.addEventListener("click", function (e) {
    var zone = e.target.closest ? e.target.closest(".paste-zone") : null;
    if (zone) zone.focus();
  });
});
