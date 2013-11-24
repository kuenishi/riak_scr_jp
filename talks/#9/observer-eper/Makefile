NAME = slide
SOURCE_DIR = source
TEPMLATE_DIR = ${SOURCE_DIR}/_templates
BUILD_DIR = build
HTML = ${BUILD_DIR}/${NAME}.html
MARKDOWN = ${SOURCE_DIR}/${NAME}.md

all: clean build

build:
	@mkdir -p ${BUILD_DIR}
	@cp -rf ${SOURCE_DIR}/images ${BUILD_DIR}/images
	slidedown ${MARKDOWN} -t ${PWD}/${TEPMLATE_DIR}/default_ja > ${HTML}

clean:
	rm -rf ${BUILD_DIR}

watch:
	watchmedo shell-command --patterns="*.md;*.erb" --wait --command="make" --recursive
