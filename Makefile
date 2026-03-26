.PHONY: test test-elm-core test-elm-review test-js-bridge test-e2e

test: test-elm-core test-elm-review test-js-bridge test-e2e

test-elm-core:
	cd packages/elm-core && npx elm-test

test-elm-review:
	cd packages/elm-review && npx elm-test

test-js-bridge:
	cd packages/js-bridge && npm test

test-e2e:
	cd demo && npm run test:e2e
