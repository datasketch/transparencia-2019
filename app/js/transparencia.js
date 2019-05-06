$(document).on('click', '.needed', function () {
  Shiny.onInputChange('last_click',this.id);
});

$(document).on('click', '.click_option', function () {
  Shiny.onInputChange('last_option',this.id);
});

$(document).on('click', '.click_ficha', function () {
  Shiny.onInputChange('last_case',this.id);
});


$(document).on('click', '.needed', function () {
var isActive = document.querySelector('.needed.active');
 if (isActive) {
    isActive.classList.remove('active')
  }
});