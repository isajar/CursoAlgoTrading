#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Nov 18 10:54:27 2020

@author: isajarspector
"""

class Padre:
    
    def __init__(self, nombre):
        self.nombre = nombre

class Hijo(Padre):
    
    def __init__(self, nombre, numero):
        Padre.__init__(self,nombre)
        self.numero = numero
        
        def __repr__(self):
         return "<Hijo(nombre='%s', numero='%s')>" % (self.nombre, self.numero)

        
class main():
    p = Padre('Juan')
    h = Hijo('Roberto',2)
    
    print(p.nombre)
    print(h)
    
if __name__ == '__main__':
    main()