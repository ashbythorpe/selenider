$(function () {
  $(".toggleable").hide();

  $("#toggle_div").click(function () {
    $(".toggleable").toggle();
  });

  $(".actions-button").on("mousedown", function (e) {
    const text = e.which === 1 ? "Left clicked" : "Right clicked";
    $("#button-output").text(text);
  });

  $(".actions-button").on("dblclick", function () {
    $("#button-output").text("Double clicked!");
  });

  $(".actions-button").hover(function () {
    $("#button-output").text("Hovered!");
  });

  $(".actions-input").on("input", function () {
    $("#text-output").text($(this).val());
  });

  $(".actions-form").on("submit", function (e) {
    e.preventDefault();
    $("#form-output").text("Form submitted!");
  });

  $("#selection").on("change", function () {
    $("#selection-output").text($(this).val());
  });

  $("#multiple-selection").on("change", function () {
    $("#multiple-selection-output").text(
      $(this)
        .val()
        .map(Number)
        .reduce((x, y) => x + y, 0),
    );
  });

  $("#editable-text").on("input", function () {
    $("#editable-text-output").text($(this).text());
  });
});
