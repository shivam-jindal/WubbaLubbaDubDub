import sys
import string
import math
import re

declared_var_types = {}

class Reflector(object):
    def __getitem__(self, name):
        return name


def evaluate_program_source(program_source):
	program_source = program_source[6:]
	program_source = program_source[0] + string.replace(program_source[1:-1], " [", ",##") + program_source[-1]
	program_source = program_source[0] + string.replace(program_source[1:-1], "],", "%,") + program_source[-1]
	program_source = string.replace(program_source, '"', "'")
	program_source = re.sub(r"[[][a-zA-Z]", '["', program_source[1:])
	program_source = re.sub(r",[a-zA-Z|#]", '","', program_source)
	program_source = string.replace(program_source[:-1], ']', '"]')
	program_source = string.replace(program_source, '"#S', '["')
	program_source = string.replace(program_source, '%"', '"]')
	#print (program_source)
	return eval(program_source)


def print_initial_program():
	print """
#include<bits/stdc++.h>
using namespace std;

int main()
{
	"""


def print_eof():
	print """
return 0;
}
	"""


def evaluate_value(statement):
	if statement[0] in ['EBoolLit','EIntLit','EVar']:
		return statement[1]
	elif statement[0] == 'EBin':
		if statement[1]  == 'Mul':
			return str(evaluate_value(statement[2])) + "*" + str(evaluate_value(statement[3]))
		elif statement[1]  == 'Add':
			return str(evaluate_value(statement[2])) + "+" + str(evaluate_value(statement[3]))
		elif statement[1]  == 'Sub':
			return str(evaluate_value(statement[2])) + "-" + str(evaluate_value(statement[3]))
		elif statement[1]  == 'Div':
			return str(evaluate_value(statement[2])) + "/" + str(evaluate_value(statement[3]))
		elif statement[1]  == 'Mod':
			return str(evaluate_value(statement[2])) + "%" + str(evaluate_value(statement[3]))
		elif statement[1]  == 'Mul':
			return str(evaluate_value(statement[2])) + "*" + str(evaluate_value(statement[3]))
		elif statement[1]  == 'Equals':
			return str(evaluate_value(statement[2])) + "==" +str(evaluate_value(statement[3]))
		elif statement[1]  == 'GreaterThan':
			return str(evaluate_value(statement[2])) + ">" + str(evaluate_value(statement[3]))
		elif statement[1]  == 'LessThan':
			return str(evaluate_value(statement[2])) + "<" + str(evaluate_value(statement[3]))
		elif statement[1]  == 'And':
			return str(evaluate_value(statement[2])) + "&&" +str(evaluate_value(statement[3]))
		elif statement[1]  == 'Or':
			return str(evaluate_value(statement[2])) + "||" +str(evaluate_value(statement[3]))
		else:
			print "[Parser Exception]: Invalid binary operation " + str(statement[0]) + str(statement[1])


def evaluate_datatype(statement):
	if statement[0] == 'EBoolLit':
		return "bool"
	elif statement[0] == 'EIntLit':
		return "int"
	elif statement[0] == 'EVar':
		return declared_var_types[statement[1]]
	elif statement[0] == 'EBin':
		if statement[1] in ['Mul', 'Add','Sub','Div','Mod','Sub']:
			return "int"
		else:
			return "bool"


def print_the_line(line):
	print line


def parse_statement(statement, list_ops=None, list2_ops=None):
	statement = statement.replace(" ", ",")
	statement = eval(statement, globals(), Reflector())
	#print statement
	if statement[0] == 'Decl':
		var_to_declare = statement[1]
		datatype = evaluate_datatype(statement[2])
		value = evaluate_value(statement[2])
		#add its type to dictionary
		declared_var_types[var_to_declare] = datatype
		print_the_line(datatype + " " + str(var_to_declare) + " = " + str(value) + ";")

	elif statement[0] == 'Print':
		print_the_line("cout<<" + str(statement[1] + "<<endl;"))
	
	elif statement[0] == 'While':
		condition = evaluate_value(statement[1])
		print_the_line("while ( " + str(condition) + " ){")
		for op in list_ops:
			print "\t",
			parse_statement(op)
		print_the_line("}")

	elif statement[0] == 'If':
		condition = evaluate_value(statement[1])
		print_the_line("if ( " + str(condition) + " ){")
		for op in list_ops:
			print "\t",
			parse_statement(op)
		print_the_line("}")
		print_the_line("else {")
		for op in list2_ops:
			print "\t",
			parse_statement(op)
		print_the_line("}")



def main():
	program_source = raw_input()
	program_source = evaluate_program_source(program_source)
	#print program_source
	print_initial_program()
	
	universe_name = program_source[0]
	index = 0
	while index <len(program_source[1]):
		if program_source[1][index].startswith("While"):
			parse_statement(program_source[1][index], program_source[1][index+1])
			index += 1
		elif program_source[1][index].startswith("If"):
			parse_statement(program_source[1][index], program_source[1][index+1], program_source[1][index+2])
			index += 2
		else:
			parse_statement(program_source[1][index])
		index += 1

	print_eof()


if __name__ == '__main__':
	main()
	exit(0)