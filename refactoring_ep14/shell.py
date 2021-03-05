from basic.runner import Runner

while True:
	text = input('basic > ')
	if text.strip() == "": continue
	result, error = Runner.run('<stdin>', text)

	if error:
		print(error.as_string())
	elif result:
		if len(result.elements) == 1:
			print(repr(result.elements[0]))
		else:
			print(repr(result))