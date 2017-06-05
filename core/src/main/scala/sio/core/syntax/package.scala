package sio.core

package object syntax {
  object all extends IOSyntax with STArraySyntax
  object io extends IOSyntax
  object st extends STSyntax
  object starray extends STArraySyntax
}
