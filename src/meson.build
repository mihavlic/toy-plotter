plotter_sources = [
  'main.c',
]

cc = meson.get_compiler('c')

deps = [
	dependency('curses'),
	cc.find_library('m', required : false)
]

executable('plotter',
  sources: plotter_sources,
  dependencies: deps
)