document.addEventListener("DOMContentLoaded", function(event) {
  'use-strict';
  Modernizr.on('webp', function (result) {
    // Get all the video tags in the DOM
    var video = document.getElementsByTagName('video');
    // Iterate through the list and get he poster attribute
    for (var i=0; i < video.length; i++) {
      var poster_url = video[i].getAttribute('poster');

      if (!result) {
        // if browser check fails then replace webp with jpg
        var jpg = poster_url.replace(/[.]webp/g,".jpg");
        video[i].setAttribute('poster', jpg)
      }
   }
  })
});