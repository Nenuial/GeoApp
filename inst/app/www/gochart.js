// When locator icon in datatable is clicked, go to that spot on the map
$(document).on("click", ".go-chart", function(e) {
  e.preventDefault();
  $el = $(this);
  var iso = $el.data("iso");
  $($("#nav a")[2]).tab("show");
  Shiny.onInputChange("demo_explorer_ui-goto", {
    iso: iso,
    nonce: Math.random()
  });
});