$(document).ready(function() {

  $("#nav").append('<li class="github"><a href="https://github.com/ahasverus/shiny-tundra-cc"><i class="fab fa-github"></i> Get Code</a></li>');

  $("#menu div").on("click", function() {
    var selection = $(this).attr("class");
    $("#color-sel").removeClass();
    $("#color-sel").addClass(selection);
    $("#color-sel").removeClass("color-list");
    var selclass = $("#color-sel").attr("class");
    $("#color").val(selclass);
    Shiny.onInputChange("color",selclass);
  });
  var selclass = $("#color-sel").attr("class");
  $("#color").val(selclass);
  Shiny.onInputChange("color",selclass);
});
