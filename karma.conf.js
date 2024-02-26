module.exports = function (config) {
    config.set({
        browsers: ['jsdom'],
        hostname: '127.0.0.1',
        basePath: 'out', // this is the same as the base-path of `:output-to` in `shadow-cljs.edn`
        files: ['test.js'], // this is the same as the file-name (ending with .js) of `:output-to` in `shadow-cljs.edn`
        frameworks: ['cljs-test'],
        plugins: ['karma-cljs-test', 'karma-jsdom-launcher'],
        colors: true,
        logLevel: config.LOG_INFO,
        client: {
            args: ["shadow.test.karma.init"],
            singleRun: true
        }
    })
};
