Module Module1

    Public LaRumeur As rumeur
    Public LePersonnage As personnage()
    Public picturebox As PictureBox()
    Public StopRumeur As Double
    Public NombreConvaincus As Double
    Public NombreRumeurRépandue As Double

    Structure coord
        Dim x As Double
        Dim y As Double
    End Structure

    Structure personnage
        Dim pos As coord
        Dim croyance As Double
        Dim persuasion As Double
        Dim credulite As Double
        Dim convaincu As Boolean
    End Structure

    Structure rumeur
        Dim credibilite As Double
    End Structure

    Sub initialiser()
        LePersonnage = New personnage(99) {}
    End Sub

    Sub RépandreRumeur()
        Dim X As Double
        For i = 0 To 99
            If LePersonnage(i).convaincu = True Then
                For j = 0 To 99
                    If (LePersonnage(j).pos.x = LePersonnage(i).pos.x And (LePersonnage(j).pos.y = LePersonnage(i).pos.y + 1 Or
                        LePersonnage(j).pos.y = LePersonnage(i).pos.y - 1)) Or (LePersonnage(j).pos.x = LePersonnage(i).pos.x - 1 And
                        (LePersonnage(j).pos.y = LePersonnage(i).pos.y + 1 Or LePersonnage(j).pos.y = LePersonnage(i).pos.y Or
                        LePersonnage(j).pos.y = LePersonnage(i).pos.y - 1)) Or (LePersonnage(j).pos.x = LePersonnage(i).pos.x + 1 And
                        (LePersonnage(j).pos.y = LePersonnage(i).pos.y + 1 Or LePersonnage(j).pos.y = LePersonnage(i).pos.y Or
                        LePersonnage(j).pos.y = LePersonnage(i).pos.y - 1)) Then
                        X = LePersonnage(j).croyance - LePersonnage(j).persuasion + LePersonnage(j).credulite + LePersonnage(i).croyance
                        X = X + LePersonnage(i).persuasion - LePersonnage(i).credulite
                        Select Case X
                            Case -15 To -13
                                LePersonnage(j).croyance = 1
                            Case -12 To -9
                                LePersonnage(j).croyance = 2
                            Case -8 To -5
                                LePersonnage(j).croyance = 3
                            Case -4 To 0
                                LePersonnage(j).croyance = 4
                            Case 1 To 6
                                LePersonnage(j).croyance = 5
                            Case 7 To 15
                                LePersonnage(j).croyance = 6
                            Case 16 To 23
                                LePersonnage(j).croyance = 7
                            Case 24 To 30
                                LePersonnage(j).croyance = 8
                            Case 31 To 37
                                LePersonnage(j).croyance = 9
                            Case 38 To 43
                                LePersonnage(j).croyance = 10

                        End Select
                        'personnages qui sont dans le cadre
                    End If

                    If LePersonnage(i).pos.x = 0 Then
                        If (LePersonnage(j).pos.y = LePersonnage(i).pos.y Or LePersonnage(j).pos.y = LePersonnage(i).pos.y + 1 Or
                            LePersonnage(j).pos.y = LePersonnage(i).pos.y - 1) And LePersonnage(j).pos.x = 9 Then
                            X = LePersonnage(j).croyance - LePersonnage(j).persuasion + LePersonnage(j).credulite
                            X = X + LePersonnage(i).croyance + LePersonnage(i).persuasion - LePersonnage(i).credulite
                            Select Case X
                                Case -15 To -13
                                    LePersonnage(j).croyance = 1
                                Case -12 To -9
                                    LePersonnage(j).croyance = 2
                                Case -8 To -5
                                    LePersonnage(j).croyance = 3
                                Case -4 To 0
                                    LePersonnage(j).croyance = 4
                                Case 1 To 6
                                    LePersonnage(j).croyance = 5
                                Case 7 To 15
                                    LePersonnage(j).croyance = 6
                                Case 16 To 23
                                    LePersonnage(j).croyance = 7
                                Case 24 To 30
                                    LePersonnage(j).croyance = 8
                                Case 31 To 37
                                    LePersonnage(j).croyance = 9
                                Case 38 To 43
                                    LePersonnage(j).croyance = 10
                            End Select
                        End If
                    End If
                    If LePersonnage(i).pos.x = 9 Then
                        If (LePersonnage(j).pos.y = LePersonnage(i).pos.y Or LePersonnage(j).pos.y = LePersonnage(i).pos.y + 1 Or
                            LePersonnage(j).pos.y = LePersonnage(i).pos.y - 1) And LePersonnage(j).pos.x = 0 Then
                            X = LePersonnage(j).croyance - LePersonnage(j).persuasion + LePersonnage(j).credulite
                            X = X + LePersonnage(i).croyance + LePersonnage(i).persuasion - LePersonnage(i).credulite
                            Select Case X
                                Case -15 To -13
                                    LePersonnage(j).croyance = 1
                                Case -12 To -9
                                    LePersonnage(j).croyance = 2
                                Case -8 To -5
                                    LePersonnage(j).croyance = 3
                                Case -4 To 0
                                    LePersonnage(j).croyance = 4
                                Case 1 To 6
                                    LePersonnage(j).croyance = 5
                                Case 7 To 15
                                    LePersonnage(j).croyance = 6
                                Case 16 To 23
                                    LePersonnage(j).croyance = 7
                                Case 24 To 30
                                    LePersonnage(j).croyance = 8
                                Case 31 To 37
                                    LePersonnage(j).croyance = 9
                                Case 38 To 43
                                    LePersonnage(j).croyance = 10
                            End Select
                        End If
                    End If
                    If LePersonnage(i).pos.y = 0 Then
                        If (LePersonnage(j).pos.x = LePersonnage(i).pos.x Or LePersonnage(j).pos.x = LePersonnage(i).pos.x + 1 Or
                            LePersonnage(j).pos.x = LePersonnage(i).pos.x - 1) And LePersonnage(j).pos.y = 9 Then
                            X = LePersonnage(j).croyance - LePersonnage(j).persuasion + LePersonnage(j).credulite
                            X = X + LePersonnage(i).croyance + LePersonnage(i).persuasion - LePersonnage(i).credulite
                            Select Case X
                                Case -15 To -13
                                    LePersonnage(j).croyance = 1
                                Case -12 To -9
                                    LePersonnage(j).croyance = 2
                                Case -8 To -5
                                    LePersonnage(j).croyance = 3
                                Case -4 To 0
                                    LePersonnage(j).croyance = 4
                                Case 1 To 6
                                    LePersonnage(j).croyance = 5
                                Case 7 To 15
                                    LePersonnage(j).croyance = 6
                                Case 16 To 23
                                    LePersonnage(j).croyance = 7
                                Case 24 To 30
                                    LePersonnage(j).croyance = 8
                                Case 31 To 37
                                    LePersonnage(j).croyance = 9
                                Case 38 To 43
                                    LePersonnage(j).croyance = 10
                            End Select
                        End If
                    End If
                    If LePersonnage(i).pos.y = 9 Then
                        If (LePersonnage(j).pos.x = LePersonnage(i).pos.x Or LePersonnage(j).pos.x = LePersonnage(i).pos.x + 1 Or
                            LePersonnage(j).pos.x = LePersonnage(i).pos.x - 1) And LePersonnage(j).pos.y = 0 Then
                            X = LePersonnage(j).croyance - LePersonnage(j).persuasion + LePersonnage(j).credulite
                            X = X + LePersonnage(i).croyance + LePersonnage(i).persuasion - LePersonnage(i).credulite
                            Select Case X
                                Case -15 To -13
                                    LePersonnage(j).croyance = 1
                                Case -12 To -9
                                    LePersonnage(j).croyance = 2
                                Case -8 To -5
                                    LePersonnage(j).croyance = 3
                                Case -4 To 0
                                    LePersonnage(j).croyance = 4
                                Case 1 To 6
                                    LePersonnage(j).croyance = 5
                                Case 7 To 15
                                    LePersonnage(j).croyance = 6
                                Case 16 To 23
                                    LePersonnage(j).croyance = 7
                                Case 24 To 30
                                    LePersonnage(j).croyance = 8
                                Case 31 To 37
                                    LePersonnage(j).croyance = 9
                                Case 38 To 43
                                    LePersonnage(j).croyance = 10
                            End Select
                            'personnages en dehors du cadre
                        End If
                    End If
                Next
            End If
        Next

    End Sub
End Module








