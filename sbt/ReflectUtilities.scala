/* sbt -- Simple Build Tool
 * Copyright 2008 David MacIver
 */
package sbt;

import scala.collection._

object ReflectUtilities{
	def camelCaseToActionName(name : String) = {
		val buffer = new StringBuilder();
		for (char <- name){
			import java.lang.Character._
			if (isUpperCase(char)){
				buffer += '-';
				buffer += toLowerCase(char);
			} else buffer += char;
		}
		buffer.toString;
	}

	def allVals[T](self : AnyRef)(implicit mt : scala.reflect.Manifest[T]) : Map[String, T] = {
		val mappings = new mutable.OpenHashMap[String, T];

		for (method <- self.getClass.getMethods){
			if ((method.getTypeParameters.length == 0) &&
					mt.erasure.isAssignableFrom(method.getReturnType) &&
					(try { self.getClass.getDeclaredField(method.getName).getType == method.getReturnType }
					 catch { case (e : java.lang.NoSuchFieldException) => false })){
				mappings(method.getName) = method.invoke(self).asInstanceOf[T];
			}
		}
	
		mappings;
	}

}
