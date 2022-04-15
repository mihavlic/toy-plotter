sources = [
  'main.c',
]

cc = meson.get_compiler('c')

deps = [
	dependency('curses'),
  # neccessary to use math.h
	cc.find_library('m', required: true)
]

executable('plotter',
  sources: sources,
  dependencies: deps,
  # we really don't care about precision, really just speeed because I have issues
  # enable avx2 vectorization
  # O3 max optimization 
  c_args: ['-ffast-math', '-mavx2', '-O3']
)