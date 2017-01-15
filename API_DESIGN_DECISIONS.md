- we do not re-expose type alias for mouse position and window size. the idea
    is that we want to minimize the surface area of the package. time and
    keycode are used because they are included in core. see issue 10 for more
    context.
