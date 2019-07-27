'use-strict'

/* A version number is useful when updating the worker logic,
   allowing you to remove outdated cache entries during the update.
*/

var gitVersion = 'c63de0b';
var pupil_docs_cache = gitVersion+"::docs-cache";

/* These resources will be downloaded and cached by the service worker
   during the installation process. If any resource fails to be downloaded,
   then the service worker won't be installed either.
*/
var urlsToCache = [
  // '/'
  '/slate/stylesheets/screen.min.css',
  '/slate/javascripts/all.min.js',
  '/slate/javascripts/plyr.min.js',
  '/slate/javascripts/yt-lazyload.min.js',
  '/videos/',
  '/images/'
];

// console.log('WORKER: Executing...');

self.addEventListener('install', function(event) {
  // console.log('WORKER: Installing...'+gitVersion)
  /* Using event.waitUntil(p) blocks the installation process on the provided
     promise. If the promise is rejected, the service worker won't be installed.
  */
  event.waitUntil(
    caches.open(pupil_docs_cache)
      .then(function(cache) {
        // console.log('Opened cache');
        return cache.addAll(urlsToCache);
      })
      // .then(function() {
        // console.log('WORKER: Install completed');
      // })
  );
});

/* The fetch event fires whenever a page controlled by this service worker requests
   a resource. This isn't limited to `fetch` or even XMLHttpRequest. Instead, it
   comprehends even the request for the HTML page on first load, as well as JS and
   CSS resources, fonts, any images, etc.
*/

self.addEventListener('fetch', function(event) {
  // console.log('WORKER: Fetch event in progress...');
  event.respondWith(
    caches.match(event.request)
      .then(function(response) {
        // Cache hit - return response
        if (response) {
          return response;
        }

        /*  IMPORTANT: Clone the request. A request is a stream and
            can only be consumed once. Since we are consuming this
            once by cache and once by the browser for fetch, we need
            to clone the response.
        */
        var fetchRequest = event.request.clone();

        return fetch(fetchRequest).then(
          function(response) {
            // Check if we received a valid response
            if(!response || response.status !== 200 || response.type !== 'basic') {
              return response;
            }

            /*  IMPORTANT: Clone the response. A response is a stream
                and because we want the browser to consume the response
                as well as the cache consuming the response, we need
                to clone it so we have two streams.
            */
            var responseToCache = response.clone();

            // console.log('WORKER: Fetch response from network.', event.request.url);

            caches.open(pupil_docs_cache)
              .then(function(cache) {
                cache.put(event.request, responseToCache);
              });

            return response;
          }
        );
      })
    );
});

/* If there is even a byte's difference in the service worker file compared
   to what it currently has, the browser considers it new. At this point the old service
   worker is still controlling the current pages so the new service worker will
   be installed and enter a waiting state. When the currently open pages of your
   site are closed, the old service worker will be killed and the new service worker
   will take control.
*/
self.addEventListener('activate', function(event) {

  // console.log('WORKER: Activate event in progress...');

  var cacheWhitelist = [pupil_docs_cache];

  event.waitUntil(
    caches
      /* This method returns a promise which will resolve to an array of available
         cache keys.
      */
      .keys()
      .then(function(cacheNames) {
        // We return a promise that settles when all outdated caches are deleted.
        return Promise.all(
          cacheNames.map(function(cacheName) {
            if (cacheWhitelist.indexOf(cacheName) === -1) {
              /* Return a promise that's fulfilled
                 when each outdated cache is deleted.
              */
              // console.log('WORKER: Outdated cache deleted');
              return caches.delete(cacheName);
            }
          })
        );
    })
  );
});