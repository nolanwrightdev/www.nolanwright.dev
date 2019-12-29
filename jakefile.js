const j = require('jake')
const pug = require('pug')
const fs = require('fs')
const childProcess = require('child_process')
const path = require('path')
const glob = require('glob')
const html = require('html-minifier')
const PurgeCSS = require('purgecss')
const CleanCSS = require('clean-css')
const browserSync = require('browser-sync')
const chokidar = require('chokidar')
const sass = require('sass')
const config = require('./config')

const read = path => fs.readFileSync(path, 'utf8')
const write = path => str => fs.writeFileSync(path, str)

j.desc('Default.')
j.task('default', function() {
	console.log('No op.')
})

j.desc('Create the build directory.')
j.directory('dist')

j.desc('Delete the build directory.')
j.task('clean', function() {
	fs.rmdirSync('dist', { recursive: true })
})

j.desc('Serve build directory.')
j.task('serve', ['dist'], async function() {
	await new Promise(resolve => {
		browserSync(
			{
				watch: true,
				server: {
					baseDir: 'dist',
					serveStaticOptions: { extensions: ['html'] },
				},
			},
			resolve,
		)
	})
	await new Promise(resolve => {
		process.on('SIGINT', () => {
			process.stdout.write('\n')
			browserSync.exit()
			resolve()
		})
	})
})

j.desc('Watch.')
j.task('watch', [], async function() {
	const watcher = chokidar.watch('{layouts,styles,website}/**/*')
	watcher.on('change', filepath => {
		if (filepath.endsWith('.pug')) {
			// ugh
			if (filepath.startsWith('layouts')) return
			const dest = filepath.replace(/^website/, 'dist').replace(/pug$/, 'html')
			buildHtml(filepath, dest)
		} else if (filepath.endsWith('.scss')) {
			write('dist/style.css')(
				sass.renderSync({
					file: 'website/style.scss',
					includePaths: [path.join(__dirname, 'node_modules')],
				}).css,
			)
			browserSync.reload()
		}
	})
	await new Promise(resolve => {
		process.on('SIGINT', () => {
			process.stdout.write('\n')
			watcher.close().then(resolve)
		})
	})
})

j.desc('Develop.')
j.task('dev', ['serve', 'watch'], { concurrency: 2 })

function buildHtml(src, dest) {
	fs.mkdirSync(require('path').dirname(dest), { recursive: true })
	write(dest)(pug.compile(read(src), { basedir: __dirname, filename: src })({}))
}

j.namespace('html', function() {
	j.rule(
		/dist\/.+\.html$/,
		x => x.replace(/^dist/, 'website').replace(/html$/, 'pug'),
		['dist'],
		function() {
			buildHtml(this.source, this.name)
		},
	)

	j.desc('Build HTML files from Pug templates.')
	j.task(
		'build',
		glob
			.sync('website/**/*.pug')
			.map(x => x.replace(/^website/, 'dist').replace(/pug$/, 'html')),
	)

	j.desc('Minify HTML files.')
	j.task('minify', ['html:build'], function() {
		const { htmlMinifierOptions } = config
		glob.sync('dist/**/*.html').forEach(p => {
			write(p)(html.minify(read(p), htmlMinifierOptions))
		})
	})
})

j.namespace('css', function() {
	j.file('dist/style.css', ['dist'], function() {
		write(this.name)(
			sass.renderSync({
				file: 'website/style.scss',
				includePaths: [path.join(__dirname, 'node_modules')],
			}).css,
		)
	})

	j.desc('Build the CSS stylesheet.')
	j.task('build', ['dist/style.css'])

	j.desc('Purge unused styles from the stylesheet.')
	j.task('purge', ['css:build', 'html:build'], function() {
		const css = {
			purge: () => {
				return new PurgeCSS({
					content: ['dist/**/*.html'],
					css: ['dist/style.css'],
				})
					.purge()
					.find(x => x.file === 'dist/style.css').css
			},
		}
		write('dist/style.css')(css.purge())
	})

	j.desc('Minify the CSS stylesheet.')
	j.task('minify', ['css:build'], function() {
		const css = {
			minify: (() => {
				const a = new CleanCSS({
					level: {
						1: { all: true },
						2: { all: true },
					},
				})
				return b => a.minify(b).styles
			})(),
		}
		write('dist/style.css')(css.minify(read('dist/style.css')))
	})
})

j.namespace('assets', function() {
	j.file('dist/logo.svg', ['dist', 'website/logo.svg'], function() {
		const [, src] = this.prereqs
		fs.copyFileSync(src, this.name)
	})

	j.file('dist/publickey.txt', ['dist'], function() {
		write(this.name)(
			childProcess.execSync('git cat-file blob nolanwrightdev-gpg-pub'),
		)
	})

	j.desc('Produce all assets.')
	j.task('all', ['dist/logo.svg', 'dist/publickey.txt'])
})
