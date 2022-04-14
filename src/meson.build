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
  c_args: ['-ffast-math']
)