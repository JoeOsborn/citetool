env LEIN_FAST_TRAMPOLINE=y lein trampoline cljsbuild auto atom-dev &
electron . > console.log & 
env LEIN_FAST_TRAMPOLINE=y lein trampoline figwheel frontend-dev