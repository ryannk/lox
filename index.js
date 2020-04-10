#!/usr/bin/env node
const path = require('path')
const fs = require('fs')
const {Elm} = require('./dist/elm.js')

const main = args => {
    
    if (args.length > 1){
        console.log('usage: lox [script]')
        process.exit(64)
    } else if (args.length == 1) {                           
        runFile(args[0]);                                      
    } else {                                                 
        runPrompt();                                           
    }

}

const runFile = filePath => {
    const fileData = fs.readFileSync(path.join(process.cwd(), filePath), {encoding: 'utf8'})
    run(fileData)
}

const run = source => {
    const app = Elm.Main.init({flags: source})
    app.ports.tokens.subscribe(console.log)
}

main(process.argv.slice(2))