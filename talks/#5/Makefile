DIST = "otp-intro.html"
SRC = "otp-intro.md"

all:
	slidedown otp-intro.md -t `pwd`/templates/default_ja > ${DIST}

clean:
	rm -f ${DIST}

watch:
	watchmedo shell-command --patterns="*.md" --wait --command="make"
