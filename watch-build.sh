while true; do
    inotifywait -e modify,create,delete -r ./src && pack --cg javascript build browser.ipkg
done