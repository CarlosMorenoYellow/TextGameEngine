# TextGameEngine

Motor de aventuras de texto desarrollado en Haskell para CI-3661.

## Integrantes

- Carlos Moreno - 21-10409
- Ricardo Garcia - 20-10274

## Variante

Al sumar los carnets sin guiones:

`2110409 + 2010274 = 4120683`

`4120683 mod 4 = 3`

Por tanto, el proyecto implementa la variante **3: Combinar**. El mundo permite unir objetos con el comando `combinar <objeto1> con <objeto2>`, siempre que exista una receta `RECETA: <objeto1> + <objeto2> -> <resultado>`.

## Compilar y ejecutar

Se necesita GHC y Cabal.

```bash
cabal update
cabal build
cabal run aventura -- mundos/mundo.txt
```

Para validar únicamente un mundo:

```bash
cabal run aventura -- --validar mundos/mundo.txt
```

También puede ejecutarse el binario generado por Cabal:

```bash
cabal exec aventura -- mundos/mundo.txt
cabal exec aventura -- --validar mundos/mundo.txt
```

## Pruebas

La suite está configurada como test-suite en Cabal:

```bash
cabal test
```

Los casos cubren el reconocimiento de comandos, el núcleo puro, mundos válidos y tres mundos inválidos.

## Mundos disponibles

- `mundos/mundo.txt`: mundo propio original para la variante Combinar.
- `mundos/invalidos/PruebaError1.txt`: errores de sintaxis V1.
- `mundos/invalidos/PruebaError2.txt`: errores semánticos V2-V6.
- `mundos/invalidos/PruebaError3.txt`: errores semánticos V7-V10 y errores propios de la variante.

Los mensajes esperados de los mundos inválidos aparecen al inicio de cada archivo como comentarios.

## Comandos

- `ir <dirección>`
- `mirar`
- `tomar <objeto>` o `coger <objeto>`
- `soltar <objeto>`
- `examinar <objeto>`
- `inventario` o `inv`
- `combinar <objeto1> con <objeto2>`
- `guardar <archivo>`
- `cargar <archivo>`
- `ayuda`
- `salir`
