# Declaración de uso de asistentes de inteligencia artificial

## Herramienta utilizada

Se utilizó un asistente de inteligencia artificial dentro de un entorno de desarrollo para apoyar tareas de edición y revisión del proyecto.

## Propósitos

El asistente ayudó a:

- Revisar los módulos existentes y resumir sus decisiones reales de implementación.
- Redactar borradores de [DECISIONES.md](DECISIONES.md).
- Revisar y corregir la estructura de [README.md](README.md).
- Identificar inconsistencias entre el código, el enunciado y los documentos entregados.
- Proponer casos de prueba y revisar errores de compilación en el momento de construir el proyecto.
- Apoyo para la creacion y revision de algunas funciones en varios archivos del motor.

El código Haskell fue desarrollado y revisado por los integrantes del equipo con apoyo de la inteligencia artificial. Las propuestas del asistente se contrastaron con el proyecto, se compilaron y se modificaron cuando no coincidían con el comportamiento requerido.

## Ejemplo concreto de una sugerencia rechazada o corregida

En un primer borrador de `DECISIONES.md`, el asistente asoció decisiones específicas con nombres obtenidos del historial de Git y agrego texto que no solicitado. Esa atribución fue rechazada porque el enunciado no exige distribuir decisiones por autor y el historial no demuestra inequívocamente quién tomó cada decisión de diseño. El borrador fue corregido eliminando todas las atribuciones personales y dejando únicamente el Contexto, las Opciones, la Decisión y las Consecuencias de cada ADR.

Otra recomendación propuesta durante el desarrollo fue considerar el uso de paquetes externos para el reconocimiento de entrada. Esta sugerencia no se aceptó porque el enunciado prohíbe librerías como Parsec, Megaparsec, Attoparsec y ReadP. El proyecto conserva su reconocimiento en `Motor.Reconocedor` mediante un `newtype` construido sobre `ReadS`.

Adicionalmente, a pesar de no ser una recomendación, la inteligencia durante sus correcciones de codigo soltaron varios errores con respecto a la sintaxis del lenguaje Haskell.
