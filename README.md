To run
1. Install elm
2. Run 'elm package install evancz/elm-html'
3. Run 'elm make Collider.elm --output collider.html'
4. Open html file in browser

Goal:
particle sandbox in elm with the following attributes modifiable on-the-fly by the user:
number of particles
particle size
movement speed

stretch goal:
some kind of "higher-order" stream, dirivitive,etc. Still thinking of ideas for this
average particle position display?

critical path:
1. Mock up/visual of user interface
2. general program outline/architecture of components
3. implementation of single particle displayed in some environment
4. implementation of users ability to create more particles and adjust their size
5. implementation of ability of particles to move
6. implementation of users ability to adjust particles speed
7. implementation of collision of particles with boundaries
8. implemenatation of collision of particles with each other

