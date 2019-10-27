module.exports = {
	content: ['dist/**/*.html'],
	css:  ['dist/tailwind.min.css'],
	extractors: [{
		extractor: class {
			static extract(content) {
				return content.match(/[\w-/:]*[\w-/:]/g) || []
			}
		},
		extensions: ['html'],
	}],
	keyframes: true,
	fontFace: true,
}
