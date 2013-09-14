exec = require("child_process").exec;

module.exports = function(grunt) {

  grunt.registerTask('foo', 'Log some stuff.', function() {
    grunt.log.write('Logging some stuff...').ok();
    grunt.log.write(this.name)
    grunt.log.write(JSON.stringify(arguments))
  });


  // Project configuration.
  grunt.initConfig({
    watch: {
      scripts: {
        files: '*.hs',
        tasks: ['foo'],
        options: {
          spawn: false,
        },
      },
    }
    
  });

  // Load the plugin that provides the "uglify" task.
  //grunt.loadNpmTasks('grunt-contrib-uglify');

  // Default task(s).
  //grunt.registerTask('default', ['uglify']);


  grunt.event.on('watch', function(action, filepath, target) {
    grunt.log.writeln('*** WATCH ***')
    grunt.log.writeln(target + ': ' + filepath + ' has ' + action);
    //if('input.hs' == filepath) {
      exec("ghc --make " + filepath);
    //}
  });

  grunt.loadNpmTasks('grunt-contrib-watch');

  grunt.registerTask('default', ['watch']);

};