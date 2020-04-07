#!/usr/bin/env node

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

main(process.argv.slice(2))