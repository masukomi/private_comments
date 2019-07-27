(function() {
  var youtube = document.querySelectorAll( ".youtube" );

  for (var i = 0; i < youtube.length; i++) {

    var img_embed = youtube[i].dataset.embed.replace(/h.+ed.|[?].+/g,'');

    var img_src = "https://img.youtube.com/vi/"+ img_embed +"/sddefault.jpg";

    // Load the image asynchronously
    var image = new Image();
      image.src = img_src;
      image.addEventListener("load", function() {
        youtube[i].appendChild(image);
      }(i));
      // click event to inject iframe
      youtube[i].addEventListener("click", function() {
        var iframe = document.createElement("iframe");

        var iframe_embed = this.dataset.embed.replace(/h.+ed.|[?].+/g,'');
        var embed_playlist = this.dataset.embed.match(/[?].+/g,'');

        iframe.setAttribute("frameborder", "0");
        iframe.setAttribute("allowfullscreen", "");

        if ( embed_playlist === null ) {
          iframe.setAttribute("src", "https://www.youtube.com/embed/"+ iframe_embed + "?rel=0&showinfo=0&autoplay=1");
        } else {
          iframe.setAttribute("src", "https://www.youtube.com/embed/"+ iframe_embed + embed_playlist + "&rel=0&showinfo=1&autoplay=1&loop=1");
        }

        this.innerHTML = "";
        this.appendChild(iframe);
      });
  };

})();