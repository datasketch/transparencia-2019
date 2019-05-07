$(document).on('click', '.needed', function () {
  Shiny.onInputChange('last_click',this.id);
});

$(document).on('click', '.click_option', function () {
  Shiny.onInputChange('last_option',this.id);
});

$(document).on('click', '.click_ficha', function () {
  Shiny.onInputChange('last_case',this.id);
});


$(document).on('click', '.needed', function(){
    $('.needed').removeClass('basic_active')
    $(this).addClass('basic_active');
});

$(document).on('click', '.click_option', function(){
  var target = event.target;
  var id = target.getAttribute('id');
  
  var avanzada = document.getElementById("cont_avanzada");
  var basicas = document.getElementById("cont_basicas");
  var flecha_adv = document.getElementById("opts_a");
  var flecha_bsc = document.getElementById("opts");
  
  
  if (id == 'Avanzada') {
    avanzada.classList.remove("hideOptions");
    basicas.classList.add("hideOptions");
    flecha_adv.classList.add("active_opt");
    flecha_bsc.classList.remove("active_opt");
  } else if (id == 'Basico') {
    basicas.classList.remove("hideOptions");
    avanzada.classList.add("hideOptions");
    flecha_adv.classList.remove("active_opt");
    flecha_bsc.classList.add("active_opt");
  } else {
    return
  }
})
