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

	def ancestry(clazz : Class[_]) : List[Class[_]] = 
		if (clazz == classOf[AnyRef] || !classOf[AnyRef].isAssignableFrom(clazz)) List(clazz)
		else clazz :: ancestry(clazz.getSuperclass);

	def fields(clazz : Class[_]) = 
		mutable.OpenHashMap(ancestry(clazz).
			flatMap(_.getDeclaredFields).
			map(f => (f.getName, f)):_*)
	
	def allVals[T](self : AnyRef)(implicit mt : scala.reflect.Manifest[T]) : Map[String, T] = {
		val mappings = new mutable.OpenHashMap[String, T];
		val correspondingFields = fields(self.getClass)

		for (method <- self.getClass.getMethods){
			if ((method.getParameterTypes.length == 0) &&
					mt.erasure.isAssignableFrom(method.getReturnType) &&
					(correspondingFields.get(method.getName) match {
							case Some(field) => field.getType == method.getReturnType
							case None => false })){
				mappings(method.getName) = method.invoke(self).asInstanceOf[T];
			}
		}
	
		mappings;
	}

}
