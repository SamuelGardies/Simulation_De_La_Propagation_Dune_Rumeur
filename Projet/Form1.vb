Public Class Form1
    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles ButtonInitialiser.Click
        Dim rand As New Random
        Dim prs As personnage
        initialiser()
        StopRumeur = 1
        NombreConvaincus = 0
        NombreRumeurRépandue = 0

        'initialisations des personnages
        For i = 0 To 99
            prs.croyance = rand.Next(1, 6)
            prs.persuasion = rand.Next(1, 11)
            prs.credulite = rand.Next(1, 11)
            prs.convaincu = False
            LePersonnage(i) = prs
        Next

        'position x et y
        For i = 0 To 9
            LePersonnage(i).pos.y = 0
            LePersonnage(i).pos.x = i
        Next
        For i = 10 To 19
            LePersonnage(i).pos.y = 1
            LePersonnage(i).pos.x = i - 10
        Next
        For i = 20 To 29
            LePersonnage(i).pos.y = 2
            LePersonnage(i).pos.x = i - 20
        Next
        For i = 30 To 39
            LePersonnage(i).pos.y = 3
            LePersonnage(i).pos.x = i - 30
        Next
        For i = 40 To 49
            LePersonnage(i).pos.y = 4
            LePersonnage(i).pos.x = i - 40
        Next
        For i = 50 To 59
            LePersonnage(i).pos.y = 5
            LePersonnage(i).pos.x = i - 50
        Next
        For i = 60 To 69
            LePersonnage(i).pos.y = 6
            LePersonnage(i).pos.x = i - 60
        Next
        For i = 70 To 79
            LePersonnage(i).pos.y = 7
            LePersonnage(i).pos.x = i - 70
        Next
        For i = 80 To 89
            LePersonnage(i).pos.y = 8
            LePersonnage(i).pos.x = i - 80
        Next
        For i = 90 To 99
            LePersonnage(i).pos.y = 9
            LePersonnage(i).pos.x = i - 90
        Next

        'initialisation du graphique
        Chart1.Series.Clear()
        Chart1.Series.Add("Series1")
        Chart1.Series("Series1").ChartType = DataVisualization.Charting.SeriesChartType.Spline
        Chart1.Series("Series1").IsValueShownAsLabel = True
        Chart1.Series("Series1").IsXValueIndexed = True

        'actualisation
        For i = 0 To 99
            Select Case LePersonnage(i).croyance
                Case 1
                    TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y).BackColor = Color.LightCyan
                Case 2
                    TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y).BackColor = Color.PaleTurquoise
                Case 3
                    TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y).BackColor = Color.LightBlue
                Case 4
                    TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y).BackColor = Color.LightSkyBlue
                Case 5
                    TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y).BackColor = Color.DeepSkyBlue
                Case 6
                    TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y).BackColor = Color.DodgerBlue
                Case 7
                    TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y).BackColor = Color.CornflowerBlue
                Case 8
                    TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y).BackColor = Color.RoyalBlue
                Case 9
                    TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y).BackColor = Color.Blue
                Case 10
                    TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y).BackColor = Color.Navy
            End Select
        Next

        For i = 0 To 99
            If LePersonnage(i).convaincu = False Then
                TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y).Cursor = Cursors.No
            End If
        Next
    End Sub

    Private Sub ButtonLancerRumeur_Click(sender As Object, e As EventArgs) Handles ButtonLancerRumeur.Click
        'initialisation de la rumeur
        LaRumeur.credibilite = TextBoxCredibilite.Text

        'messages d'erreur
        If TextBoxCredibilite.Text < 1 Or TextBoxCredibilite.Text > 10 Then
            MsgBox("la crédibilité  de la rumeur doit être un entier compris entre 1 et 10")
        End If
        If TextBoxPosRumeurX.Text < 0 Or TextBoxPosRumeurX.Text > 9 Then
            MsgBox("la position du personnage qui lance la rumeur doit être un entier compris entre 0 et 9")
        End If
        If TextBoxPosRumeurY.Text < 0 Or TextBoxPosRumeurY.Text > 9 Then
            MsgBox("la position du personnage qui lance la rumeur doit être un entier compris entre 0 et 9")
        End If

        'personnage qui va répandre la rumeur
        For i = 0 To 99
            If LePersonnage(i).pos.x = TextBoxPosRumeurX.Text And LePersonnage(i).pos.y = TextBoxPosRumeurY.Text Then
                If LePersonnage(i).convaincu = False Then
                    NombreConvaincus = NombreConvaincus + 1   'si le personnage est pas déjà convaincus alors +1 personnage convaincu
                End If

                Select Case (LePersonnage(i).credulite + LaRumeur.credibilite)
                    Case 2 To 5
                        LePersonnage(i).croyance = 6
                    Case 6 To 9
                        LePersonnage(i).croyance = 7
                    Case 10 To 13
                        LePersonnage(i).croyance = 8
                    Case 14 To 17
                        LePersonnage(i).croyance = 9
                    Case 18 To 20
                        LePersonnage(i).croyance = 10
                End Select
            End If
        Next

        'actualisation

        For i = 0 To 99
            TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y).Cursor = DefaultCursor
        Next

        For i = 0 To 99
            If LePersonnage(i).croyance > 5 Then
                LePersonnage(i).convaincu = True
            End If
        Next

        For i = 0 To 99
            Select Case LePersonnage(i).croyance
                Case 1
                    TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y).BackColor = Color.LightCyan
                Case 2
                    TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y).BackColor = Color.PaleTurquoise
                Case 3
                    TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y).BackColor = Color.LightBlue
                Case 4
                    TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y).BackColor = Color.LightSkyBlue
                Case 5
                    TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y).BackColor = Color.DeepSkyBlue
                Case 6
                    TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y).BackColor = Color.DodgerBlue
                Case 7
                    TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y).BackColor = Color.CornflowerBlue
                Case 8
                    TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y).BackColor = Color.RoyalBlue
                Case 9
                    TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y).BackColor = Color.Blue
                Case 10
                    TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y).BackColor = Color.Navy
            End Select
        Next

        For i = 0 To 99
            If LePersonnage(i).convaincu = False Then
                TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y).Cursor = Cursors.No
            End If
        Next

        'actualisation graphique
        NombreRumeurRépandue = NombreRumeurRépandue + 1
        Chart1.Series("Series1").Points.AddXY(NombreRumeurRépandue, NombreConvaincus)
    End Sub

    Private Sub ButtonRépandre_Click(sender As Object, e As EventArgs) Handles ButtonRépandre.Click
        Dim c As Double
        c = 0
        RépandreRumeur()

        'actualisation

        For i = 0 To 99
            TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y).Cursor = DefaultCursor
        Next

        For i = 0 To 99
            If LePersonnage(i).croyance > 5 Then
                LePersonnage(i).convaincu = True
            Else
                LePersonnage(i).convaincu = False
            End If
        Next

        For i = 0 To 99
            Select Case LePersonnage(i).croyance
                Case 1
                    TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y).BackColor = Color.LightCyan
                Case 2
                    TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y).BackColor = Color.PaleTurquoise
                Case 3
                    TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y).BackColor = Color.LightBlue
                Case 4
                    TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y).BackColor = Color.LightSkyBlue
                Case 5
                    TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y).BackColor = Color.DeepSkyBlue
                Case 6
                    TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y).BackColor = Color.DodgerBlue
                Case 7
                    TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y).BackColor = Color.CornflowerBlue
                Case 8
                    TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y).BackColor = Color.RoyalBlue
                Case 9
                    TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y).BackColor = Color.Blue
                Case 10
                    TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y).BackColor = Color.Navy
            End Select
        Next

        For i = 0 To 99
            If LePersonnage(i).convaincu = False Then
                TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y).Cursor = Cursors.No
            End If
        Next

        'actualisation graphique
        For i = 0 To 99
            If LePersonnage(i).convaincu = True Then
                c = c + 1                                   'pour éviter de recompter les personnages déjà convaincus
            End If
        Next
        NombreConvaincus = c
        NombreRumeurRépandue = NombreRumeurRépandue + 1
        Chart1.Series("Series1").Points.AddXY(NombreRumeurRépandue, NombreConvaincus)
    End Sub

    Private Sub ButtonStop_Click(sender As Object, e As EventArgs) Handles ButtonStop.Click
        StopRumeur = 1
    End Sub

    Private Async Sub ButtonVRAI_Click(sender As Object, e As EventArgs) Handles ButtonVRAI.Click
        StopRumeur = 0

        Select Case TrackBar1.Value
            Case 0
                ButtonRépandre_Click(ButtonVRAI, e)
            Case 1
                Do While StopRumeur = 0
                    ButtonRépandre_Click(ButtonVRAI, e)
                    Await Task.Delay(2000)
                Loop
            Case 2
                Do While StopRumeur = 0
                    ButtonRépandre_Click(ButtonVRAI, e)
                    Await Task.Delay(1000)
                Loop
        End Select

    End Sub

    Private Sub ButtonGraphique_Click(sender As Object, e As EventArgs) Handles ButtonGraphique.Click
        Chart1.BringToFront()
        ButtonGrille.BringToFront()
    End Sub

    Private Sub ButtonGrille_Click(sender As Object, e As EventArgs) Handles ButtonGrille.Click
        TableLayoutPanel1.BringToFront()
        ButtonGraphique.BringToFront()
    End Sub

    Private Sub TrackBar1_MouseHover(sender As Object, e As EventArgs) Handles TrackBar1.MouseHover
        Dim tooltip1 As New ToolTip()
        tooltip1.ToolTipTitle = "Vitesses possibles :"
        tooltip1.SetToolTip(TrackBar1, "- Rapide
- Lent
- Pas à pas")
    End Sub

    Private Sub Chart1_MouseHover(sender As Object, e As EventArgs) Handles Chart1.MouseHover
        Dim tooltip1 As New ToolTip()
        tooltip1.ToolTipTitle = "Remarque :"
        tooltip1.SetToolTip(Chart1, "Un personnage est considéré comme convaincu si la valeur de sa croyance est comprise entre 6 et 10 inclus")
    End Sub

    Private Sub ButtonGraphique_MouseHover(sender As Object, e As EventArgs) Handles ButtonGraphique.MouseHover
        Dim tooltip1 As New ToolTip()
        tooltip1.ToolTipTitle = "Cliquer pour afficher le Graphique"
        tooltip1.SetToolTip(ButtonGraphique, " ")
    End Sub

    Private Sub ButtonGrille_MouseHover(sender As Object, e As EventArgs) Handles ButtonGrille.MouseHover
        Dim tooltip1 As New ToolTip()
        tooltip1.ToolTipTitle = "Cliquer pour afficher les personnages"
        tooltip1.SetToolTip(ButtonGrille, " ")
    End Sub



    Private Sub PictureBox1_Click(sender As Object, e As EventArgs) Handles PictureBox1.Click
        Dim lesautrespicturebox As PictureBox
        TextBoxPosX.Text = LePersonnage(0).pos.x
        TextBoxPosY.Text = LePersonnage(0).pos.y
        TextBoxCroyance.Text = LePersonnage(0).croyance
        TextBoxPersuasion.Text = LePersonnage(0).persuasion
        TextBoxCredulite.Text = LePersonnage(0).credulite
        TextBoxConvaincu.Text = LePersonnage(0).convaincu
        If TextBoxConvaincu.Text = "False" Then
            TextBoxConvaincu.Text = "Non"
        ElseIf TextBoxConvaincu.Text = "True" Then
            TextBoxConvaincu.Text = "Oui"
        End If
        For i = 0 To 99
            lesautrespicturebox = TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y)
            lesautrespicturebox.BorderStyle = BorderStyle.None
        Next
        PictureBox1.BorderStyle = BorderStyle.Fixed3D
        PictureBox102.BackColor = PictureBox1.BackColor
    End Sub

    Private Sub PictureBox2_Click(sender As Object, e As EventArgs) Handles PictureBox2.Click
        Dim lesautrespicturebox As PictureBox
        TextBoxPosX.Text = LePersonnage(1).pos.x
        TextBoxPosY.Text = LePersonnage(1).pos.y
        TextBoxCroyance.Text = LePersonnage(1).croyance
        TextBoxPersuasion.Text = LePersonnage(1).persuasion
        TextBoxCredulite.Text = LePersonnage(1).credulite
        TextBoxConvaincu.Text = LePersonnage(1).convaincu
        If TextBoxConvaincu.Text = "False" Then
            TextBoxConvaincu.Text = "Non"
        ElseIf TextBoxConvaincu.Text = "True" Then
            TextBoxConvaincu.Text = "Oui"
        End If
        For i = 0 To 99
            lesautrespicturebox = TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y)
            lesautrespicturebox.BorderStyle = BorderStyle.None
        Next
        PictureBox2.BorderStyle = BorderStyle.Fixed3D
        PictureBox102.BackColor = PictureBox2.BackColor
    End Sub

    Private Sub PictureBox3_Click(sender As Object, e As EventArgs) Handles PictureBox3.Click
        Dim lesautrespicturebox As PictureBox
        TextBoxPosX.Text = LePersonnage(2).pos.x
        TextBoxPosY.Text = LePersonnage(2).pos.y
        TextBoxCroyance.Text = LePersonnage(2).croyance
        TextBoxPersuasion.Text = LePersonnage(2).persuasion
        TextBoxCredulite.Text = LePersonnage(2).credulite
        TextBoxConvaincu.Text = LePersonnage(2).convaincu
        If TextBoxConvaincu.Text = "False" Then
            TextBoxConvaincu.Text = "Non"
        ElseIf TextBoxConvaincu.Text = "True" Then
            TextBoxConvaincu.Text = "Oui"
        End If
        For i = 0 To 99
            lesautrespicturebox = TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y)
            lesautrespicturebox.BorderStyle = BorderStyle.None
        Next
        PictureBox3.BorderStyle = BorderStyle.Fixed3D
        PictureBox102.BackColor = PictureBox3.BackColor
    End Sub

    Private Sub PictureBox4_Click(sender As Object, e As EventArgs) Handles PictureBox4.Click
        Dim lesautrespicturebox As PictureBox
        TextBoxPosX.Text = LePersonnage(3).pos.x
        TextBoxPosY.Text = LePersonnage(3).pos.y
        TextBoxCroyance.Text = LePersonnage(3).croyance
        TextBoxPersuasion.Text = LePersonnage(3).persuasion
        TextBoxCredulite.Text = LePersonnage(3).credulite
        TextBoxConvaincu.Text = LePersonnage(3).convaincu
        If TextBoxConvaincu.Text = "False" Then
            TextBoxConvaincu.Text = "Non"
        ElseIf TextBoxConvaincu.Text = "True" Then
            TextBoxConvaincu.Text = "Oui"
        End If
        For i = 0 To 99
            lesautrespicturebox = TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y)
            lesautrespicturebox.BorderStyle = BorderStyle.None
        Next
        PictureBox4.BorderStyle = BorderStyle.Fixed3D
        PictureBox102.BackColor = PictureBox4.BackColor
    End Sub

    Private Sub PictureBox5_Click(sender As Object, e As EventArgs) Handles PictureBox5.Click
        Dim lesautrespicturebox As PictureBox
        TextBoxPosX.Text = LePersonnage(4).pos.x
        TextBoxPosY.Text = LePersonnage(4).pos.y
        TextBoxCroyance.Text = LePersonnage(4).croyance
        TextBoxPersuasion.Text = LePersonnage(4).persuasion
        TextBoxCredulite.Text = LePersonnage(4).credulite
        TextBoxConvaincu.Text = LePersonnage(4).convaincu
        If TextBoxConvaincu.Text = "False" Then
            TextBoxConvaincu.Text = "Non"
        ElseIf TextBoxConvaincu.Text = "True" Then
            TextBoxConvaincu.Text = "Oui"
        End If
        For i = 0 To 99
            lesautrespicturebox = TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y)
            lesautrespicturebox.BorderStyle = BorderStyle.None
        Next
        PictureBox5.BorderStyle = BorderStyle.Fixed3D
        PictureBox102.BackColor = PictureBox5.BackColor
    End Sub

    Private Sub PictureBox6_Click(sender As Object, e As EventArgs) Handles PictureBox6.Click
        Dim lesautrespicturebox As PictureBox
        TextBoxPosX.Text = LePersonnage(5).pos.x
        TextBoxPosY.Text = LePersonnage(5).pos.y
        TextBoxCroyance.Text = LePersonnage(5).croyance
        TextBoxPersuasion.Text = LePersonnage(5).persuasion
        TextBoxCredulite.Text = LePersonnage(5).credulite
        TextBoxConvaincu.Text = LePersonnage(5).convaincu
        If TextBoxConvaincu.Text = "False" Then
            TextBoxConvaincu.Text = "Non"
        ElseIf TextBoxConvaincu.Text = "True" Then
            TextBoxConvaincu.Text = "Oui"
        End If
        For i = 0 To 99
            lesautrespicturebox = TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y)
            lesautrespicturebox.BorderStyle = BorderStyle.None
        Next
        PictureBox6.BorderStyle = BorderStyle.Fixed3D
        PictureBox102.BackColor = PictureBox6.BackColor
    End Sub

    Private Sub PictureBox7_Click(sender As Object, e As EventArgs) Handles PictureBox7.Click
        Dim lesautrespicturebox As PictureBox
        TextBoxPosX.Text = LePersonnage(6).pos.x
        TextBoxPosY.Text = LePersonnage(6).pos.y
        TextBoxCroyance.Text = LePersonnage(6).croyance
        TextBoxPersuasion.Text = LePersonnage(6).persuasion
        TextBoxCredulite.Text = LePersonnage(6).credulite
        TextBoxConvaincu.Text = LePersonnage(6).convaincu
        If TextBoxConvaincu.Text = "False" Then
            TextBoxConvaincu.Text = "Non"
        ElseIf TextBoxConvaincu.Text = "True" Then
            TextBoxConvaincu.Text = "Oui"
        End If
        For i = 0 To 99
            lesautrespicturebox = TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y)
            lesautrespicturebox.BorderStyle = BorderStyle.None
        Next
        PictureBox7.BorderStyle = BorderStyle.Fixed3D
        PictureBox102.BackColor = PictureBox7.BackColor
    End Sub

    Private Sub PictureBox8_Click(sender As Object, e As EventArgs) Handles PictureBox8.Click
        Dim lesautrespicturebox As PictureBox
        TextBoxPosX.Text = LePersonnage(7).pos.x
        TextBoxPosY.Text = LePersonnage(7).pos.y
        TextBoxCroyance.Text = LePersonnage(7).croyance
        TextBoxPersuasion.Text = LePersonnage(7).persuasion
        TextBoxCredulite.Text = LePersonnage(7).credulite
        TextBoxConvaincu.Text = LePersonnage(7).convaincu
        If TextBoxConvaincu.Text = "False" Then
            TextBoxConvaincu.Text = "Non"
        ElseIf TextBoxConvaincu.Text = "True" Then
            TextBoxConvaincu.Text = "Oui"
        End If
        For i = 0 To 99
            lesautrespicturebox = TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y)
            lesautrespicturebox.BorderStyle = BorderStyle.None
        Next
        PictureBox8.BorderStyle = BorderStyle.Fixed3D
        PictureBox102.BackColor = PictureBox8.BackColor
    End Sub

    Private Sub PictureBox9_Click(sender As Object, e As EventArgs) Handles PictureBox9.Click
        Dim lesautrespicturebox As PictureBox
        TextBoxPosX.Text = LePersonnage(8).pos.x
        TextBoxPosY.Text = LePersonnage(8).pos.y
        TextBoxCroyance.Text = LePersonnage(8).croyance
        TextBoxPersuasion.Text = LePersonnage(8).persuasion
        TextBoxCredulite.Text = LePersonnage(8).credulite
        TextBoxConvaincu.Text = LePersonnage(8).convaincu
        If TextBoxConvaincu.Text = "False" Then
            TextBoxConvaincu.Text = "Non"
        ElseIf TextBoxConvaincu.Text = "True" Then
            TextBoxConvaincu.Text = "Oui"
        End If
        For i = 0 To 99
            lesautrespicturebox = TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y)
            lesautrespicturebox.BorderStyle = BorderStyle.None
        Next
        PictureBox9.BorderStyle = BorderStyle.Fixed3D
        PictureBox102.BackColor = PictureBox9.BackColor
    End Sub

    Private Sub PictureBox10_Click(sender As Object, e As EventArgs) Handles PictureBox10.Click
        Dim lesautrespicturebox As PictureBox
        TextBoxPosX.Text = LePersonnage(9).pos.x
        TextBoxPosY.Text = LePersonnage(9).pos.y
        TextBoxCroyance.Text = LePersonnage(9).croyance
        TextBoxPersuasion.Text = LePersonnage(9).persuasion
        TextBoxCredulite.Text = LePersonnage(9).credulite
        TextBoxConvaincu.Text = LePersonnage(9).convaincu
        If TextBoxConvaincu.Text = "False" Then
            TextBoxConvaincu.Text = "Non"
        ElseIf TextBoxConvaincu.Text = "True" Then
            TextBoxConvaincu.Text = "Oui"
        End If
        For i = 0 To 99
            lesautrespicturebox = TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y)
            lesautrespicturebox.BorderStyle = BorderStyle.None
        Next
        PictureBox10.BorderStyle = BorderStyle.Fixed3D
        PictureBox102.BackColor = PictureBox10.BackColor
    End Sub

    Private Sub PictureBox11_Click(sender As Object, e As EventArgs) Handles PictureBox11.Click
        Dim lesautrespicturebox As PictureBox
        TextBoxPosX.Text = LePersonnage(10).pos.x
        TextBoxPosY.Text = LePersonnage(10).pos.y
        TextBoxCroyance.Text = LePersonnage(10).croyance
        TextBoxPersuasion.Text = LePersonnage(10).persuasion
        TextBoxCredulite.Text = LePersonnage(10).credulite
        TextBoxConvaincu.Text = LePersonnage(10).convaincu
        If TextBoxConvaincu.Text = "False" Then
            TextBoxConvaincu.Text = "Non"
        ElseIf TextBoxConvaincu.Text = "True" Then
            TextBoxConvaincu.Text = "Oui"
        End If
        For i = 0 To 99
            lesautrespicturebox = TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y)
            lesautrespicturebox.BorderStyle = BorderStyle.None
        Next
        PictureBox11.BorderStyle = BorderStyle.Fixed3D
        PictureBox102.BackColor = PictureBox11.BackColor
    End Sub

    Private Sub PictureBox12_Click(sender As Object, e As EventArgs) Handles PictureBox12.Click
        Dim lesautrespicturebox As PictureBox
        TextBoxPosX.Text = LePersonnage(11).pos.x
        TextBoxPosY.Text = LePersonnage(11).pos.y
        TextBoxCroyance.Text = LePersonnage(11).croyance
        TextBoxPersuasion.Text = LePersonnage(11).persuasion
        TextBoxCredulite.Text = LePersonnage(11).credulite
        TextBoxConvaincu.Text = LePersonnage(11).convaincu
        If TextBoxConvaincu.Text = "False" Then
            TextBoxConvaincu.Text = "Non"
        ElseIf TextBoxConvaincu.Text = "True" Then
            TextBoxConvaincu.Text = "Oui"
        End If
        For i = 0 To 99
            lesautrespicturebox = TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y)
            lesautrespicturebox.BorderStyle = BorderStyle.None
        Next
        PictureBox12.BorderStyle = BorderStyle.Fixed3D
        PictureBox102.BackColor = PictureBox12.BackColor
    End Sub

    Private Sub PictureBox13_Click(sender As Object, e As EventArgs) Handles PictureBox13.Click
        Dim lesautrespicturebox As PictureBox
        TextBoxPosX.Text = LePersonnage(12).pos.x
        TextBoxPosY.Text = LePersonnage(12).pos.y
        TextBoxCroyance.Text = LePersonnage(12).croyance
        TextBoxPersuasion.Text = LePersonnage(12).persuasion
        TextBoxCredulite.Text = LePersonnage(12).credulite
        TextBoxConvaincu.Text = LePersonnage(12).convaincu
        If TextBoxConvaincu.Text = "False" Then
            TextBoxConvaincu.Text = "Non"
        ElseIf TextBoxConvaincu.Text = "True" Then
            TextBoxConvaincu.Text = "Oui"
        End If
        For i = 0 To 99
            lesautrespicturebox = TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y)
            lesautrespicturebox.BorderStyle = BorderStyle.None
        Next
        PictureBox13.BorderStyle = BorderStyle.Fixed3D
        PictureBox102.BackColor = PictureBox13.BackColor
    End Sub

    Private Sub PictureBox14_Click(sender As Object, e As EventArgs) Handles PictureBox14.Click
        Dim lesautrespicturebox As PictureBox
        TextBoxPosX.Text = LePersonnage(13).pos.x
        TextBoxPosY.Text = LePersonnage(13).pos.y
        TextBoxCroyance.Text = LePersonnage(13).croyance
        TextBoxPersuasion.Text = LePersonnage(13).persuasion
        TextBoxCredulite.Text = LePersonnage(13).credulite
        TextBoxConvaincu.Text = LePersonnage(13).convaincu
        If TextBoxConvaincu.Text = "False" Then
            TextBoxConvaincu.Text = "Non"
        ElseIf TextBoxConvaincu.Text = "True" Then
            TextBoxConvaincu.Text = "Oui"
        End If
        For i = 0 To 99
            lesautrespicturebox = TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y)
            lesautrespicturebox.BorderStyle = BorderStyle.None
        Next
        PictureBox14.BorderStyle = BorderStyle.Fixed3D
        PictureBox102.BackColor = PictureBox14.BackColor
    End Sub

    Private Sub PictureBox15_Click(sender As Object, e As EventArgs) Handles PictureBox15.Click
        Dim lesautrespicturebox As PictureBox
        TextBoxPosX.Text = LePersonnage(14).pos.x
        TextBoxPosY.Text = LePersonnage(14).pos.y
        TextBoxCroyance.Text = LePersonnage(14).croyance
        TextBoxPersuasion.Text = LePersonnage(14).persuasion
        TextBoxCredulite.Text = LePersonnage(14).credulite
        TextBoxConvaincu.Text = LePersonnage(14).convaincu
        If TextBoxConvaincu.Text = "False" Then
            TextBoxConvaincu.Text = "Non"
        ElseIf TextBoxConvaincu.Text = "True" Then
            TextBoxConvaincu.Text = "Oui"
        End If
        For i = 0 To 99
            lesautrespicturebox = TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y)
            lesautrespicturebox.BorderStyle = BorderStyle.None
        Next
        PictureBox15.BorderStyle = BorderStyle.Fixed3D
        PictureBox102.BackColor = PictureBox15.BackColor
    End Sub

    Private Sub PictureBox16_Click(sender As Object, e As EventArgs) Handles PictureBox16.Click
        Dim lesautrespicturebox As PictureBox
        TextBoxPosX.Text = LePersonnage(15).pos.x
        TextBoxPosY.Text = LePersonnage(15).pos.y
        TextBoxCroyance.Text = LePersonnage(15).croyance
        TextBoxPersuasion.Text = LePersonnage(15).persuasion
        TextBoxCredulite.Text = LePersonnage(15).credulite
        TextBoxConvaincu.Text = LePersonnage(15).convaincu
        If TextBoxConvaincu.Text = "False" Then
            TextBoxConvaincu.Text = "Non"
        ElseIf TextBoxConvaincu.Text = "True" Then
            TextBoxConvaincu.Text = "Oui"
        End If
        For i = 0 To 99
            lesautrespicturebox = TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y)
            lesautrespicturebox.BorderStyle = BorderStyle.None
        Next
        PictureBox16.BorderStyle = BorderStyle.Fixed3D
        PictureBox102.BackColor = PictureBox16.BackColor
    End Sub

    Private Sub PictureBox17_Click(sender As Object, e As EventArgs) Handles PictureBox17.Click
        Dim lesautrespicturebox As PictureBox
        TextBoxPosX.Text = LePersonnage(16).pos.x
        TextBoxPosY.Text = LePersonnage(16).pos.y
        TextBoxCroyance.Text = LePersonnage(16).croyance
        TextBoxPersuasion.Text = LePersonnage(16).persuasion
        TextBoxCredulite.Text = LePersonnage(16).credulite
        TextBoxConvaincu.Text = LePersonnage(16).convaincu
        If TextBoxConvaincu.Text = "False" Then
            TextBoxConvaincu.Text = "Non"
        ElseIf TextBoxConvaincu.Text = "True" Then
            TextBoxConvaincu.Text = "Oui"
        End If
        For i = 0 To 99
            lesautrespicturebox = TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y)
            lesautrespicturebox.BorderStyle = BorderStyle.None
        Next
        PictureBox17.BorderStyle = BorderStyle.Fixed3D
        PictureBox102.BackColor = PictureBox17.BackColor
    End Sub

    Private Sub PictureBox18_Click(sender As Object, e As EventArgs) Handles PictureBox18.Click
        Dim lesautrespicturebox As PictureBox
        TextBoxPosX.Text = LePersonnage(17).pos.x
        TextBoxPosY.Text = LePersonnage(17).pos.y
        TextBoxCroyance.Text = LePersonnage(17).croyance
        TextBoxPersuasion.Text = LePersonnage(17).persuasion
        TextBoxCredulite.Text = LePersonnage(17).credulite
        TextBoxConvaincu.Text = LePersonnage(17).convaincu
        If TextBoxConvaincu.Text = "False" Then
            TextBoxConvaincu.Text = "Non"
        ElseIf TextBoxConvaincu.Text = "True" Then
            TextBoxConvaincu.Text = "Oui"
        End If
        For i = 0 To 99
            lesautrespicturebox = TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y)
            lesautrespicturebox.BorderStyle = BorderStyle.None
        Next
        PictureBox18.BorderStyle = BorderStyle.Fixed3D
        PictureBox102.BackColor = PictureBox18.BackColor
    End Sub

    Private Sub PictureBox19_Click(sender As Object, e As EventArgs) Handles PictureBox19.Click
        Dim lesautrespicturebox As PictureBox
        TextBoxPosX.Text = LePersonnage(18).pos.x
        TextBoxPosY.Text = LePersonnage(18).pos.y
        TextBoxCroyance.Text = LePersonnage(18).croyance
        TextBoxPersuasion.Text = LePersonnage(18).persuasion
        TextBoxCredulite.Text = LePersonnage(18).credulite
        TextBoxConvaincu.Text = LePersonnage(18).convaincu
        If TextBoxConvaincu.Text = "False" Then
            TextBoxConvaincu.Text = "Non"
        ElseIf TextBoxConvaincu.Text = "True" Then
            TextBoxConvaincu.Text = "Oui"
        End If
        For i = 0 To 99
            lesautrespicturebox = TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y)
            lesautrespicturebox.BorderStyle = BorderStyle.None
        Next
        PictureBox19.BorderStyle = BorderStyle.Fixed3D
        PictureBox102.BackColor = PictureBox19.BackColor
    End Sub

    Private Sub PictureBox20_Click(sender As Object, e As EventArgs) Handles PictureBox20.Click
        Dim lesautrespicturebox As PictureBox
        TextBoxPosX.Text = LePersonnage(19).pos.x
        TextBoxPosY.Text = LePersonnage(19).pos.y
        TextBoxCroyance.Text = LePersonnage(19).croyance
        TextBoxPersuasion.Text = LePersonnage(19).persuasion
        TextBoxCredulite.Text = LePersonnage(19).credulite
        TextBoxConvaincu.Text = LePersonnage(19).convaincu
        If TextBoxConvaincu.Text = "False" Then
            TextBoxConvaincu.Text = "Non"
        ElseIf TextBoxConvaincu.Text = "True" Then
            TextBoxConvaincu.Text = "Oui"
        End If
        For i = 0 To 99
            lesautrespicturebox = TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y)
            lesautrespicturebox.BorderStyle = BorderStyle.None
        Next
        PictureBox20.BorderStyle = BorderStyle.Fixed3D
        PictureBox102.BackColor = PictureBox20.BackColor
    End Sub

    Private Sub PictureBox21_Click(sender As Object, e As EventArgs) Handles PictureBox21.Click
        Dim lesautrespicturebox As PictureBox
        TextBoxPosX.Text = LePersonnage(20).pos.x
        TextBoxPosY.Text = LePersonnage(20).pos.y
        TextBoxCroyance.Text = LePersonnage(20).croyance
        TextBoxPersuasion.Text = LePersonnage(20).persuasion
        TextBoxCredulite.Text = LePersonnage(20).credulite
        TextBoxConvaincu.Text = LePersonnage(20).convaincu
        If TextBoxConvaincu.Text = "False" Then
            TextBoxConvaincu.Text = "Non"
        ElseIf TextBoxConvaincu.Text = "True" Then
            TextBoxConvaincu.Text = "Oui"
        End If
        For i = 0 To 99
            lesautrespicturebox = TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y)
            lesautrespicturebox.BorderStyle = BorderStyle.None
        Next
        PictureBox21.BorderStyle = BorderStyle.Fixed3D
        PictureBox102.BackColor = PictureBox21.BackColor
    End Sub

    Private Sub PictureBox22_Click(sender As Object, e As EventArgs) Handles PictureBox22.Click
        Dim lesautrespicturebox As PictureBox
        TextBoxPosX.Text = LePersonnage(21).pos.x
        TextBoxPosY.Text = LePersonnage(21).pos.y
        TextBoxCroyance.Text = LePersonnage(21).croyance
        TextBoxPersuasion.Text = LePersonnage(21).persuasion
        TextBoxCredulite.Text = LePersonnage(21).credulite
        TextBoxConvaincu.Text = LePersonnage(21).convaincu
        If TextBoxConvaincu.Text = "False" Then
            TextBoxConvaincu.Text = "Non"
        ElseIf TextBoxConvaincu.Text = "True" Then
            TextBoxConvaincu.Text = "Oui"
        End If
        For i = 0 To 99
            lesautrespicturebox = TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y)
            lesautrespicturebox.BorderStyle = BorderStyle.None
        Next
        PictureBox22.BorderStyle = BorderStyle.Fixed3D
        PictureBox102.BackColor = PictureBox22.BackColor
    End Sub

    Private Sub PictureBox23_Click(sender As Object, e As EventArgs) Handles PictureBox23.Click
        Dim lesautrespicturebox As PictureBox
        TextBoxPosX.Text = LePersonnage(22).pos.x
        TextBoxPosY.Text = LePersonnage(22).pos.y
        TextBoxCroyance.Text = LePersonnage(22).croyance
        TextBoxPersuasion.Text = LePersonnage(22).persuasion
        TextBoxCredulite.Text = LePersonnage(22).credulite
        TextBoxConvaincu.Text = LePersonnage(22).convaincu
        If TextBoxConvaincu.Text = "False" Then
            TextBoxConvaincu.Text = "Non"
        ElseIf TextBoxConvaincu.Text = "True" Then
            TextBoxConvaincu.Text = "Oui"
        End If
        For i = 0 To 99
            lesautrespicturebox = TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y)
            lesautrespicturebox.BorderStyle = BorderStyle.None
        Next
        PictureBox23.BorderStyle = BorderStyle.Fixed3D
        PictureBox102.BackColor = PictureBox23.BackColor
    End Sub

    Private Sub PictureBox24_Click(sender As Object, e As EventArgs) Handles PictureBox24.Click
        Dim lesautrespicturebox As PictureBox
        TextBoxPosX.Text = LePersonnage(23).pos.x
        TextBoxPosY.Text = LePersonnage(23).pos.y
        TextBoxCroyance.Text = LePersonnage(23).croyance
        TextBoxPersuasion.Text = LePersonnage(23).persuasion
        TextBoxCredulite.Text = LePersonnage(23).credulite
        TextBoxConvaincu.Text = LePersonnage(23).convaincu
        If TextBoxConvaincu.Text = "False" Then
            TextBoxConvaincu.Text = "Non"
        ElseIf TextBoxConvaincu.Text = "True" Then
            TextBoxConvaincu.Text = "Oui"
        End If
        For i = 0 To 99
            lesautrespicturebox = TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y)
            lesautrespicturebox.BorderStyle = BorderStyle.None
        Next
        PictureBox24.BorderStyle = BorderStyle.Fixed3D
        PictureBox102.BackColor = PictureBox24.BackColor
    End Sub

    Private Sub PictureBox25_Click(sender As Object, e As EventArgs) Handles PictureBox25.Click
        Dim lesautrespicturebox As PictureBox
        TextBoxPosX.Text = LePersonnage(24).pos.x
        TextBoxPosY.Text = LePersonnage(24).pos.y
        TextBoxCroyance.Text = LePersonnage(24).croyance
        TextBoxPersuasion.Text = LePersonnage(24).persuasion
        TextBoxCredulite.Text = LePersonnage(24).credulite
        TextBoxConvaincu.Text = LePersonnage(24).convaincu
        If TextBoxConvaincu.Text = "False" Then
            TextBoxConvaincu.Text = "Non"
        ElseIf TextBoxConvaincu.Text = "True" Then
            TextBoxConvaincu.Text = "Oui"
        End If
        For i = 0 To 99
            lesautrespicturebox = TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y)
            lesautrespicturebox.BorderStyle = BorderStyle.None
        Next
        PictureBox25.BorderStyle = BorderStyle.Fixed3D
        PictureBox102.BackColor = PictureBox25.BackColor
    End Sub

    Private Sub PictureBox26_Click(sender As Object, e As EventArgs) Handles PictureBox26.Click
        Dim lesautrespicturebox As PictureBox
        TextBoxPosX.Text = LePersonnage(25).pos.x
        TextBoxPosY.Text = LePersonnage(25).pos.y
        TextBoxCroyance.Text = LePersonnage(25).croyance
        TextBoxPersuasion.Text = LePersonnage(25).persuasion
        TextBoxCredulite.Text = LePersonnage(25).credulite
        TextBoxConvaincu.Text = LePersonnage(25).convaincu
        If TextBoxConvaincu.Text = "False" Then
            TextBoxConvaincu.Text = "Non"
        ElseIf TextBoxConvaincu.Text = "True" Then
            TextBoxConvaincu.Text = "Oui"
        End If
        For i = 0 To 99
            lesautrespicturebox = TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y)
            lesautrespicturebox.BorderStyle = BorderStyle.None
        Next
        PictureBox26.BorderStyle = BorderStyle.Fixed3D
        PictureBox102.BackColor = PictureBox26.BackColor
    End Sub

    Private Sub PictureBox27_Click(sender As Object, e As EventArgs) Handles PictureBox27.Click
        Dim lesautrespicturebox As PictureBox
        TextBoxPosX.Text = LePersonnage(26).pos.x
        TextBoxPosY.Text = LePersonnage(26).pos.y
        TextBoxCroyance.Text = LePersonnage(26).croyance
        TextBoxPersuasion.Text = LePersonnage(26).persuasion
        TextBoxCredulite.Text = LePersonnage(26).credulite
        TextBoxConvaincu.Text = LePersonnage(26).convaincu
        If TextBoxConvaincu.Text = "False" Then
            TextBoxConvaincu.Text = "Non"
        ElseIf TextBoxConvaincu.Text = "True" Then
            TextBoxConvaincu.Text = "Oui"
        End If
        For i = 0 To 99
            lesautrespicturebox = TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y)
            lesautrespicturebox.BorderStyle = BorderStyle.None
        Next
        PictureBox27.BorderStyle = BorderStyle.Fixed3D
        PictureBox102.BackColor = PictureBox27.BackColor
    End Sub

    Private Sub PictureBox28_Click(sender As Object, e As EventArgs) Handles PictureBox28.Click
        Dim lesautrespicturebox As PictureBox
        TextBoxPosX.Text = LePersonnage(27).pos.x
        TextBoxPosY.Text = LePersonnage(27).pos.y
        TextBoxCroyance.Text = LePersonnage(27).croyance
        TextBoxPersuasion.Text = LePersonnage(27).persuasion
        TextBoxCredulite.Text = LePersonnage(27).credulite
        TextBoxConvaincu.Text = LePersonnage(27).convaincu
        If TextBoxConvaincu.Text = "False" Then
            TextBoxConvaincu.Text = "Non"
        ElseIf TextBoxConvaincu.Text = "True" Then
            TextBoxConvaincu.Text = "Oui"
        End If
        For i = 0 To 99
            lesautrespicturebox = TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y)
            lesautrespicturebox.BorderStyle = BorderStyle.None
        Next
        PictureBox28.BorderStyle = BorderStyle.Fixed3D
        PictureBox102.BackColor = PictureBox28.BackColor
    End Sub

    Private Sub PictureBox29_Click(sender As Object, e As EventArgs) Handles PictureBox29.Click
        Dim lesautrespicturebox As PictureBox
        TextBoxPosX.Text = LePersonnage(28).pos.x
        TextBoxPosY.Text = LePersonnage(28).pos.y
        TextBoxCroyance.Text = LePersonnage(28).croyance
        TextBoxPersuasion.Text = LePersonnage(28).persuasion
        TextBoxCredulite.Text = LePersonnage(28).credulite
        TextBoxConvaincu.Text = LePersonnage(28).convaincu
        If TextBoxConvaincu.Text = "False" Then
            TextBoxConvaincu.Text = "Non"
        ElseIf TextBoxConvaincu.Text = "True" Then
            TextBoxConvaincu.Text = "Oui"
        End If
        For i = 0 To 99
            lesautrespicturebox = TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y)
            lesautrespicturebox.BorderStyle = BorderStyle.None
        Next
        PictureBox29.BorderStyle = BorderStyle.Fixed3D
        PictureBox102.BackColor = PictureBox29.BackColor
    End Sub

    Private Sub PictureBox30_Click(sender As Object, e As EventArgs) Handles PictureBox30.Click
        Dim lesautrespicturebox As PictureBox
        TextBoxPosX.Text = LePersonnage(29).pos.x
        TextBoxPosY.Text = LePersonnage(29).pos.y
        TextBoxCroyance.Text = LePersonnage(29).croyance
        TextBoxPersuasion.Text = LePersonnage(29).persuasion
        TextBoxCredulite.Text = LePersonnage(29).credulite
        TextBoxConvaincu.Text = LePersonnage(29).convaincu
        If TextBoxConvaincu.Text = "False" Then
            TextBoxConvaincu.Text = "Non"
        ElseIf TextBoxConvaincu.Text = "True" Then
            TextBoxConvaincu.Text = "Oui"
        End If
        For i = 0 To 99
            lesautrespicturebox = TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y)
            lesautrespicturebox.BorderStyle = BorderStyle.None
        Next
        PictureBox30.BorderStyle = BorderStyle.Fixed3D
        PictureBox102.BackColor = PictureBox30.BackColor
    End Sub

    Private Sub PictureBox31_Click(sender As Object, e As EventArgs) Handles PictureBox31.Click
        Dim lesautrespicturebox As PictureBox
        TextBoxPosX.Text = LePersonnage(30).pos.x
        TextBoxPosY.Text = LePersonnage(30).pos.y
        TextBoxCroyance.Text = LePersonnage(30).croyance
        TextBoxPersuasion.Text = LePersonnage(30).persuasion
        TextBoxCredulite.Text = LePersonnage(30).credulite
        TextBoxConvaincu.Text = LePersonnage(30).convaincu
        If TextBoxConvaincu.Text = "False" Then
            TextBoxConvaincu.Text = "Non"
        ElseIf TextBoxConvaincu.Text = "True" Then
            TextBoxConvaincu.Text = "Oui"
        End If
        For i = 0 To 99
            lesautrespicturebox = TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y)
            lesautrespicturebox.BorderStyle = BorderStyle.None
        Next
        PictureBox31.BorderStyle = BorderStyle.Fixed3D
        PictureBox102.BackColor = PictureBox31.BackColor
    End Sub

    Private Sub PictureBox32_Click(sender As Object, e As EventArgs) Handles PictureBox32.Click
        Dim lesautrespicturebox As PictureBox
        TextBoxPosX.Text = LePersonnage(31).pos.x
        TextBoxPosY.Text = LePersonnage(31).pos.y
        TextBoxCroyance.Text = LePersonnage(31).croyance
        TextBoxPersuasion.Text = LePersonnage(31).persuasion
        TextBoxCredulite.Text = LePersonnage(31).credulite
        TextBoxConvaincu.Text = LePersonnage(31).convaincu
        If TextBoxConvaincu.Text = "False" Then
            TextBoxConvaincu.Text = "Non"
        ElseIf TextBoxConvaincu.Text = "True" Then
            TextBoxConvaincu.Text = "Oui"
        End If
        For i = 0 To 99
            lesautrespicturebox = TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y)
            lesautrespicturebox.BorderStyle = BorderStyle.None
        Next
        PictureBox32.BorderStyle = BorderStyle.Fixed3D
        PictureBox102.BackColor = PictureBox32.BackColor
    End Sub

    Private Sub PictureBox33_Click(sender As Object, e As EventArgs) Handles PictureBox33.Click
        Dim lesautrespicturebox As PictureBox
        TextBoxPosX.Text = LePersonnage(32).pos.x
        TextBoxPosY.Text = LePersonnage(32).pos.y
        TextBoxCroyance.Text = LePersonnage(32).croyance
        TextBoxPersuasion.Text = LePersonnage(32).persuasion
        TextBoxCredulite.Text = LePersonnage(32).credulite
        TextBoxConvaincu.Text = LePersonnage(32).convaincu
        If TextBoxConvaincu.Text = "False" Then
            TextBoxConvaincu.Text = "Non"
        ElseIf TextBoxConvaincu.Text = "True" Then
            TextBoxConvaincu.Text = "Oui"
        End If
        For i = 0 To 99
            lesautrespicturebox = TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y)
            lesautrespicturebox.BorderStyle = BorderStyle.None
        Next
        PictureBox33.BorderStyle = BorderStyle.Fixed3D
        PictureBox102.BackColor = PictureBox33.BackColor
    End Sub

    Private Sub PictureBox34_Click(sender As Object, e As EventArgs) Handles PictureBox34.Click
        Dim lesautrespicturebox As PictureBox
        TextBoxPosX.Text = LePersonnage(33).pos.x
        TextBoxPosY.Text = LePersonnage(33).pos.y
        TextBoxCroyance.Text = LePersonnage(33).croyance
        TextBoxPersuasion.Text = LePersonnage(33).persuasion
        TextBoxCredulite.Text = LePersonnage(33).credulite
        TextBoxConvaincu.Text = LePersonnage(33).convaincu
        If TextBoxConvaincu.Text = "False" Then
            TextBoxConvaincu.Text = "Non"
        ElseIf TextBoxConvaincu.Text = "True" Then
            TextBoxConvaincu.Text = "Oui"
        End If
        For i = 0 To 99
            lesautrespicturebox = TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y)
            lesautrespicturebox.BorderStyle = BorderStyle.None
        Next
        PictureBox34.BorderStyle = BorderStyle.Fixed3D
        PictureBox102.BackColor = PictureBox34.BackColor
    End Sub

    Private Sub PictureBox35_Click(sender As Object, e As EventArgs) Handles PictureBox35.Click
        Dim lesautrespicturebox As PictureBox
        TextBoxPosX.Text = LePersonnage(34).pos.x
        TextBoxPosY.Text = LePersonnage(34).pos.y
        TextBoxCroyance.Text = LePersonnage(34).croyance
        TextBoxPersuasion.Text = LePersonnage(34).persuasion
        TextBoxCredulite.Text = LePersonnage(34).credulite
        TextBoxConvaincu.Text = LePersonnage(34).convaincu
        If TextBoxConvaincu.Text = "False" Then
            TextBoxConvaincu.Text = "Non"
        ElseIf TextBoxConvaincu.Text = "True" Then
            TextBoxConvaincu.Text = "Oui"
        End If
        For i = 0 To 99
            lesautrespicturebox = TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y)
            lesautrespicturebox.BorderStyle = BorderStyle.None
        Next
        PictureBox35.BorderStyle = BorderStyle.Fixed3D
        PictureBox102.BackColor = PictureBox35.BackColor
    End Sub

    Private Sub PictureBox36_Click(sender As Object, e As EventArgs) Handles PictureBox36.Click
        Dim lesautrespicturebox As PictureBox
        TextBoxPosX.Text = LePersonnage(35).pos.x
        TextBoxPosY.Text = LePersonnage(35).pos.y
        TextBoxCroyance.Text = LePersonnage(35).croyance
        TextBoxPersuasion.Text = LePersonnage(35).persuasion
        TextBoxCredulite.Text = LePersonnage(35).credulite
        TextBoxConvaincu.Text = LePersonnage(35).convaincu
        If TextBoxConvaincu.Text = "False" Then
            TextBoxConvaincu.Text = "Non"
        ElseIf TextBoxConvaincu.Text = "True" Then
            TextBoxConvaincu.Text = "Oui"
        End If
        For i = 0 To 99
            lesautrespicturebox = TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y)
            lesautrespicturebox.BorderStyle = BorderStyle.None
        Next
        PictureBox36.BorderStyle = BorderStyle.Fixed3D
        PictureBox102.BackColor = PictureBox36.BackColor
    End Sub

    Private Sub PictureBox37_Click(sender As Object, e As EventArgs) Handles PictureBox37.Click
        Dim lesautrespicturebox As PictureBox
        TextBoxPosX.Text = LePersonnage(36).pos.x
        TextBoxPosY.Text = LePersonnage(36).pos.y
        TextBoxCroyance.Text = LePersonnage(36).croyance
        TextBoxPersuasion.Text = LePersonnage(36).persuasion
        TextBoxCredulite.Text = LePersonnage(36).credulite
        TextBoxConvaincu.Text = LePersonnage(36).convaincu
        If TextBoxConvaincu.Text = "False" Then
            TextBoxConvaincu.Text = "Non"
        ElseIf TextBoxConvaincu.Text = "True" Then
            TextBoxConvaincu.Text = "Oui"
        End If
        For i = 0 To 99
            lesautrespicturebox = TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y)
            lesautrespicturebox.BorderStyle = BorderStyle.None
        Next
        PictureBox37.BorderStyle = BorderStyle.Fixed3D
        PictureBox102.BackColor = PictureBox37.BackColor
    End Sub

    Private Sub PictureBox38_Click(sender As Object, e As EventArgs) Handles PictureBox38.Click
        Dim lesautrespicturebox As PictureBox
        TextBoxPosX.Text = LePersonnage(37).pos.x
        TextBoxPosY.Text = LePersonnage(37).pos.y
        TextBoxCroyance.Text = LePersonnage(37).croyance
        TextBoxPersuasion.Text = LePersonnage(37).persuasion
        TextBoxCredulite.Text = LePersonnage(37).credulite
        TextBoxConvaincu.Text = LePersonnage(37).convaincu
        If TextBoxConvaincu.Text = "False" Then
            TextBoxConvaincu.Text = "Non"
        ElseIf TextBoxConvaincu.Text = "True" Then
            TextBoxConvaincu.Text = "Oui"
        End If
        For i = 0 To 99
            lesautrespicturebox = TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y)
            lesautrespicturebox.BorderStyle = BorderStyle.None
        Next
        PictureBox38.BorderStyle = BorderStyle.Fixed3D
        PictureBox102.BackColor = PictureBox38.BackColor
    End Sub

    Private Sub PictureBox39_Click(sender As Object, e As EventArgs) Handles PictureBox39.Click
        Dim lesautrespicturebox As PictureBox
        TextBoxPosX.Text = LePersonnage(38).pos.x
        TextBoxPosY.Text = LePersonnage(38).pos.y
        TextBoxCroyance.Text = LePersonnage(38).croyance
        TextBoxPersuasion.Text = LePersonnage(38).persuasion
        TextBoxCredulite.Text = LePersonnage(38).credulite
        TextBoxConvaincu.Text = LePersonnage(38).convaincu
        If TextBoxConvaincu.Text = "False" Then
            TextBoxConvaincu.Text = "Non"
        ElseIf TextBoxConvaincu.Text = "True" Then
            TextBoxConvaincu.Text = "Oui"
        End If
        For i = 0 To 99
            lesautrespicturebox = TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y)
            lesautrespicturebox.BorderStyle = BorderStyle.None
        Next
        PictureBox39.BorderStyle = BorderStyle.Fixed3D
        PictureBox102.BackColor = PictureBox39.BackColor
    End Sub

    Private Sub PictureBox40_Click(sender As Object, e As EventArgs) Handles PictureBox40.Click
        Dim lesautrespicturebox As PictureBox
        TextBoxPosX.Text = LePersonnage(39).pos.x
        TextBoxPosY.Text = LePersonnage(39).pos.y
        TextBoxCroyance.Text = LePersonnage(39).croyance
        TextBoxPersuasion.Text = LePersonnage(39).persuasion
        TextBoxCredulite.Text = LePersonnage(39).credulite
        TextBoxConvaincu.Text = LePersonnage(39).convaincu
        If TextBoxConvaincu.Text = "False" Then
            TextBoxConvaincu.Text = "Non"
        ElseIf TextBoxConvaincu.Text = "True" Then
            TextBoxConvaincu.Text = "Oui"
        End If
        For i = 0 To 99
            lesautrespicturebox = TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y)
            lesautrespicturebox.BorderStyle = BorderStyle.None
        Next
        PictureBox40.BorderStyle = BorderStyle.Fixed3D
        PictureBox102.BackColor = PictureBox40.BackColor
    End Sub

    Private Sub PictureBox41_Click(sender As Object, e As EventArgs) Handles PictureBox41.Click
        Dim lesautrespicturebox As PictureBox
        TextBoxPosX.Text = LePersonnage(40).pos.x
        TextBoxPosY.Text = LePersonnage(40).pos.y
        TextBoxCroyance.Text = LePersonnage(40).croyance
        TextBoxPersuasion.Text = LePersonnage(40).persuasion
        TextBoxCredulite.Text = LePersonnage(40).credulite
        TextBoxConvaincu.Text = LePersonnage(40).convaincu
        If TextBoxConvaincu.Text = "False" Then
            TextBoxConvaincu.Text = "Non"
        ElseIf TextBoxConvaincu.Text = "True" Then
            TextBoxConvaincu.Text = "Oui"
        End If
        For i = 0 To 99
            lesautrespicturebox = TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y)
            lesautrespicturebox.BorderStyle = BorderStyle.None
        Next
        PictureBox41.BorderStyle = BorderStyle.Fixed3D
        PictureBox102.BackColor = PictureBox41.BackColor
    End Sub

    Private Sub PictureBox42_Click(sender As Object, e As EventArgs) Handles PictureBox42.Click
        Dim lesautrespicturebox As PictureBox
        TextBoxPosX.Text = LePersonnage(41).pos.x
        TextBoxPosY.Text = LePersonnage(41).pos.y
        TextBoxCroyance.Text = LePersonnage(41).croyance
        TextBoxPersuasion.Text = LePersonnage(41).persuasion
        TextBoxCredulite.Text = LePersonnage(41).credulite
        TextBoxConvaincu.Text = LePersonnage(41).convaincu
        If TextBoxConvaincu.Text = "False" Then
            TextBoxConvaincu.Text = "Non"
        ElseIf TextBoxConvaincu.Text = "True" Then
            TextBoxConvaincu.Text = "Oui"
        End If
        For i = 0 To 99
            lesautrespicturebox = TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y)
            lesautrespicturebox.BorderStyle = BorderStyle.None
        Next
        PictureBox42.BorderStyle = BorderStyle.Fixed3D
        PictureBox102.BackColor = PictureBox42.BackColor
    End Sub

    Private Sub PictureBox43_Click(sender As Object, e As EventArgs) Handles PictureBox43.Click
        Dim lesautrespicturebox As PictureBox
        TextBoxPosX.Text = LePersonnage(42).pos.x
        TextBoxPosY.Text = LePersonnage(42).pos.y
        TextBoxCroyance.Text = LePersonnage(42).croyance
        TextBoxPersuasion.Text = LePersonnage(42).persuasion
        TextBoxCredulite.Text = LePersonnage(42).credulite
        TextBoxConvaincu.Text = LePersonnage(42).convaincu
        If TextBoxConvaincu.Text = "False" Then
            TextBoxConvaincu.Text = "Non"
        ElseIf TextBoxConvaincu.Text = "True" Then
            TextBoxConvaincu.Text = "Oui"
        End If
        For i = 0 To 99
            lesautrespicturebox = TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y)
            lesautrespicturebox.BorderStyle = BorderStyle.None
        Next
        PictureBox43.BorderStyle = BorderStyle.Fixed3D
        PictureBox102.BackColor = PictureBox43.BackColor
    End Sub

    Private Sub PictureBox44_Click(sender As Object, e As EventArgs) Handles PictureBox44.Click
        Dim lesautrespicturebox As PictureBox
        TextBoxPosX.Text = LePersonnage(43).pos.x
        TextBoxPosY.Text = LePersonnage(43).pos.y
        TextBoxCroyance.Text = LePersonnage(43).croyance
        TextBoxPersuasion.Text = LePersonnage(43).persuasion
        TextBoxCredulite.Text = LePersonnage(43).credulite
        TextBoxConvaincu.Text = LePersonnage(43).convaincu
        If TextBoxConvaincu.Text = "False" Then
            TextBoxConvaincu.Text = "Non"
        ElseIf TextBoxConvaincu.Text = "True" Then
            TextBoxConvaincu.Text = "Oui"
        End If
        For i = 0 To 99
            lesautrespicturebox = TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y)
            lesautrespicturebox.BorderStyle = BorderStyle.None
        Next
        PictureBox44.BorderStyle = BorderStyle.Fixed3D
        PictureBox102.BackColor = PictureBox44.BackColor
    End Sub

    Private Sub PictureBox45_Click(sender As Object, e As EventArgs) Handles PictureBox45.Click
        Dim lesautrespicturebox As PictureBox
        TextBoxPosX.Text = LePersonnage(44).pos.x
        TextBoxPosY.Text = LePersonnage(44).pos.y
        TextBoxCroyance.Text = LePersonnage(44).croyance
        TextBoxPersuasion.Text = LePersonnage(44).persuasion
        TextBoxCredulite.Text = LePersonnage(44).credulite
        TextBoxConvaincu.Text = LePersonnage(44).convaincu
        If TextBoxConvaincu.Text = "False" Then
            TextBoxConvaincu.Text = "Non"
        ElseIf TextBoxConvaincu.Text = "True" Then
            TextBoxConvaincu.Text = "Oui"
        End If
        For i = 0 To 99
            lesautrespicturebox = TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y)
            lesautrespicturebox.BorderStyle = BorderStyle.None
        Next
        PictureBox45.BorderStyle = BorderStyle.Fixed3D
        PictureBox102.BackColor = PictureBox45.BackColor
    End Sub

    Private Sub PictureBox46_Click(sender As Object, e As EventArgs) Handles PictureBox46.Click
        Dim lesautrespicturebox As PictureBox
        TextBoxPosX.Text = LePersonnage(45).pos.x
        TextBoxPosY.Text = LePersonnage(45).pos.y
        TextBoxCroyance.Text = LePersonnage(45).croyance
        TextBoxPersuasion.Text = LePersonnage(45).persuasion
        TextBoxCredulite.Text = LePersonnage(45).credulite
        TextBoxConvaincu.Text = LePersonnage(45).convaincu
        If TextBoxConvaincu.Text = "False" Then
            TextBoxConvaincu.Text = "Non"
        ElseIf TextBoxConvaincu.Text = "True" Then
            TextBoxConvaincu.Text = "Oui"
        End If
        For i = 0 To 99
            lesautrespicturebox = TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y)
            lesautrespicturebox.BorderStyle = BorderStyle.None
        Next
        PictureBox46.BorderStyle = BorderStyle.Fixed3D
        PictureBox102.BackColor = PictureBox46.BackColor
    End Sub

    Private Sub PictureBox47_Click(sender As Object, e As EventArgs) Handles PictureBox47.Click
        Dim lesautrespicturebox As PictureBox
        TextBoxPosX.Text = LePersonnage(46).pos.x
        TextBoxPosY.Text = LePersonnage(46).pos.y
        TextBoxCroyance.Text = LePersonnage(46).croyance
        TextBoxPersuasion.Text = LePersonnage(46).persuasion
        TextBoxCredulite.Text = LePersonnage(46).credulite
        TextBoxConvaincu.Text = LePersonnage(46).convaincu
        If TextBoxConvaincu.Text = "False" Then
            TextBoxConvaincu.Text = "Non"
        ElseIf TextBoxConvaincu.Text = "True" Then
            TextBoxConvaincu.Text = "Oui"
        End If
        For i = 0 To 99
            lesautrespicturebox = TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y)
            lesautrespicturebox.BorderStyle = BorderStyle.None
        Next
        PictureBox47.BorderStyle = BorderStyle.Fixed3D
        PictureBox102.BackColor = PictureBox47.BackColor
    End Sub

    Private Sub PictureBox48_Click(sender As Object, e As EventArgs) Handles PictureBox48.Click
        Dim lesautrespicturebox As PictureBox
        TextBoxPosX.Text = LePersonnage(47).pos.x
        TextBoxPosY.Text = LePersonnage(47).pos.y
        TextBoxCroyance.Text = LePersonnage(47).croyance
        TextBoxPersuasion.Text = LePersonnage(47).persuasion
        TextBoxCredulite.Text = LePersonnage(47).credulite
        TextBoxConvaincu.Text = LePersonnage(47).convaincu
        If TextBoxConvaincu.Text = "False" Then
            TextBoxConvaincu.Text = "Non"
        ElseIf TextBoxConvaincu.Text = "True" Then
            TextBoxConvaincu.Text = "Oui"
        End If
        For i = 0 To 99
            lesautrespicturebox = TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y)
            lesautrespicturebox.BorderStyle = BorderStyle.None
        Next
        PictureBox48.BorderStyle = BorderStyle.Fixed3D
        PictureBox102.BackColor = PictureBox48.BackColor
    End Sub

    Private Sub PictureBox49_Click(sender As Object, e As EventArgs) Handles PictureBox49.Click
        Dim lesautrespicturebox As PictureBox
        TextBoxPosX.Text = LePersonnage(48).pos.x
        TextBoxPosY.Text = LePersonnage(48).pos.y
        TextBoxCroyance.Text = LePersonnage(48).croyance
        TextBoxPersuasion.Text = LePersonnage(48).persuasion
        TextBoxCredulite.Text = LePersonnage(48).credulite
        TextBoxConvaincu.Text = LePersonnage(48).convaincu
        If TextBoxConvaincu.Text = "False" Then
            TextBoxConvaincu.Text = "Non"
        ElseIf TextBoxConvaincu.Text = "True" Then
            TextBoxConvaincu.Text = "Oui"
        End If
        For i = 0 To 99
            lesautrespicturebox = TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y)
            lesautrespicturebox.BorderStyle = BorderStyle.None
        Next
        PictureBox49.BorderStyle = BorderStyle.Fixed3D
        PictureBox102.BackColor = PictureBox49.BackColor
    End Sub

    Private Sub PictureBox50_Click(sender As Object, e As EventArgs) Handles PictureBox50.Click
        Dim lesautrespicturebox As PictureBox
        TextBoxPosX.Text = LePersonnage(49).pos.x
        TextBoxPosY.Text = LePersonnage(49).pos.y
        TextBoxCroyance.Text = LePersonnage(49).croyance
        TextBoxPersuasion.Text = LePersonnage(49).persuasion
        TextBoxCredulite.Text = LePersonnage(49).credulite
        TextBoxConvaincu.Text = LePersonnage(49).convaincu
        If TextBoxConvaincu.Text = "False" Then
            TextBoxConvaincu.Text = "Non"
        ElseIf TextBoxConvaincu.Text = "True" Then
            TextBoxConvaincu.Text = "Oui"
        End If
        For i = 0 To 99
            lesautrespicturebox = TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y)
            lesautrespicturebox.BorderStyle = BorderStyle.None
        Next
        PictureBox50.BorderStyle = BorderStyle.Fixed3D
        PictureBox102.BackColor = PictureBox50.BackColor
    End Sub

    Private Sub PictureBox51_Click(sender As Object, e As EventArgs) Handles PictureBox51.Click
        Dim lesautrespicturebox As PictureBox
        TextBoxPosX.Text = LePersonnage(50).pos.x
        TextBoxPosY.Text = LePersonnage(50).pos.y
        TextBoxCroyance.Text = LePersonnage(50).croyance
        TextBoxPersuasion.Text = LePersonnage(50).persuasion
        TextBoxCredulite.Text = LePersonnage(50).credulite
        TextBoxConvaincu.Text = LePersonnage(50).convaincu
        If TextBoxConvaincu.Text = "False" Then
            TextBoxConvaincu.Text = "Non"
        ElseIf TextBoxConvaincu.Text = "True" Then
            TextBoxConvaincu.Text = "Oui"
        End If
        For i = 0 To 99
            lesautrespicturebox = TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y)
            lesautrespicturebox.BorderStyle = BorderStyle.None
        Next
        PictureBox51.BorderStyle = BorderStyle.Fixed3D
        PictureBox102.BackColor = PictureBox51.BackColor
    End Sub

    Private Sub PictureBox52_Click(sender As Object, e As EventArgs) Handles PictureBox52.Click
        Dim lesautrespicturebox As PictureBox
        TextBoxPosX.Text = LePersonnage(51).pos.x
        TextBoxPosY.Text = LePersonnage(51).pos.y
        TextBoxCroyance.Text = LePersonnage(51).croyance
        TextBoxPersuasion.Text = LePersonnage(51).persuasion
        TextBoxCredulite.Text = LePersonnage(51).credulite
        TextBoxConvaincu.Text = LePersonnage(51).convaincu
        If TextBoxConvaincu.Text = "False" Then
            TextBoxConvaincu.Text = "Non"
        ElseIf TextBoxConvaincu.Text = "True" Then
            TextBoxConvaincu.Text = "Oui"
        End If
        For i = 0 To 99
            lesautrespicturebox = TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y)
            lesautrespicturebox.BorderStyle = BorderStyle.None
        Next
        PictureBox52.BorderStyle = BorderStyle.Fixed3D
        PictureBox102.BackColor = PictureBox52.BackColor
    End Sub

    Private Sub PictureBox53_Click(sender As Object, e As EventArgs) Handles PictureBox53.Click
        Dim lesautrespicturebox As PictureBox
        TextBoxPosX.Text = LePersonnage(52).pos.x
        TextBoxPosY.Text = LePersonnage(52).pos.y
        TextBoxCroyance.Text = LePersonnage(52).croyance
        TextBoxPersuasion.Text = LePersonnage(52).persuasion
        TextBoxCredulite.Text = LePersonnage(52).credulite
        TextBoxConvaincu.Text = LePersonnage(52).convaincu
        If TextBoxConvaincu.Text = "False" Then
            TextBoxConvaincu.Text = "Non"
        ElseIf TextBoxConvaincu.Text = "True" Then
            TextBoxConvaincu.Text = "Oui"
        End If
        For i = 0 To 99
            lesautrespicturebox = TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y)
            lesautrespicturebox.BorderStyle = BorderStyle.None
        Next
        PictureBox53.BorderStyle = BorderStyle.Fixed3D
        PictureBox102.BackColor = PictureBox53.BackColor
    End Sub

    Private Sub PictureBox54_Click(sender As Object, e As EventArgs) Handles PictureBox54.Click
        Dim lesautrespicturebox As PictureBox
        TextBoxPosX.Text = LePersonnage(53).pos.x
        TextBoxPosY.Text = LePersonnage(53).pos.y
        TextBoxCroyance.Text = LePersonnage(53).croyance
        TextBoxPersuasion.Text = LePersonnage(53).persuasion
        TextBoxCredulite.Text = LePersonnage(53).credulite
        TextBoxConvaincu.Text = LePersonnage(53).convaincu
        If TextBoxConvaincu.Text = "False" Then
            TextBoxConvaincu.Text = "Non"
        ElseIf TextBoxConvaincu.Text = "True" Then
            TextBoxConvaincu.Text = "Oui"
        End If
        For i = 0 To 99
            lesautrespicturebox = TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y)
            lesautrespicturebox.BorderStyle = BorderStyle.None
        Next
        PictureBox54.BorderStyle = BorderStyle.Fixed3D
        PictureBox102.BackColor = PictureBox54.BackColor
    End Sub

    Private Sub PictureBox55_Click(sender As Object, e As EventArgs) Handles PictureBox55.Click
        Dim lesautrespicturebox As PictureBox
        TextBoxPosX.Text = LePersonnage(54).pos.x
        TextBoxPosY.Text = LePersonnage(54).pos.y
        TextBoxCroyance.Text = LePersonnage(54).croyance
        TextBoxPersuasion.Text = LePersonnage(54).persuasion
        TextBoxCredulite.Text = LePersonnage(54).credulite
        TextBoxConvaincu.Text = LePersonnage(54).convaincu
        If TextBoxConvaincu.Text = "False" Then
            TextBoxConvaincu.Text = "Non"
        ElseIf TextBoxConvaincu.Text = "True" Then
            TextBoxConvaincu.Text = "Oui"
        End If
        For i = 0 To 99
            lesautrespicturebox = TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y)
            lesautrespicturebox.BorderStyle = BorderStyle.None
        Next
        PictureBox55.BorderStyle = BorderStyle.Fixed3D
        PictureBox102.BackColor = PictureBox55.BackColor
    End Sub

    Private Sub PictureBox56_Click(sender As Object, e As EventArgs) Handles PictureBox56.Click
        Dim lesautrespicturebox As PictureBox
        TextBoxPosX.Text = LePersonnage(55).pos.x
        TextBoxPosY.Text = LePersonnage(55).pos.y
        TextBoxCroyance.Text = LePersonnage(55).croyance
        TextBoxPersuasion.Text = LePersonnage(55).persuasion
        TextBoxCredulite.Text = LePersonnage(55).credulite
        TextBoxConvaincu.Text = LePersonnage(55).convaincu
        If TextBoxConvaincu.Text = "False" Then
            TextBoxConvaincu.Text = "Non"
        ElseIf TextBoxConvaincu.Text = "True" Then
            TextBoxConvaincu.Text = "Oui"
        End If
        For i = 0 To 99
            lesautrespicturebox = TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y)
            lesautrespicturebox.BorderStyle = BorderStyle.None
        Next
        PictureBox56.BorderStyle = BorderStyle.Fixed3D
        PictureBox102.BackColor = PictureBox56.BackColor
    End Sub

    Private Sub PictureBox57_Click(sender As Object, e As EventArgs) Handles PictureBox57.Click
        Dim lesautrespicturebox As PictureBox
        TextBoxPosX.Text = LePersonnage(56).pos.x
        TextBoxPosY.Text = LePersonnage(56).pos.y
        TextBoxCroyance.Text = LePersonnage(56).croyance
        TextBoxPersuasion.Text = LePersonnage(56).persuasion
        TextBoxCredulite.Text = LePersonnage(56).credulite
        TextBoxConvaincu.Text = LePersonnage(56).convaincu
        If TextBoxConvaincu.Text = "False" Then
            TextBoxConvaincu.Text = "Non"
        ElseIf TextBoxConvaincu.Text = "True" Then
            TextBoxConvaincu.Text = "Oui"
        End If
        For i = 0 To 99
            lesautrespicturebox = TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y)
            lesautrespicturebox.BorderStyle = BorderStyle.None
        Next
        PictureBox57.BorderStyle = BorderStyle.Fixed3D
        PictureBox102.BackColor = PictureBox57.BackColor
    End Sub

    Private Sub PictureBox58_Click(sender As Object, e As EventArgs) Handles PictureBox58.Click
        Dim lesautrespicturebox As PictureBox
        TextBoxPosX.Text = LePersonnage(57).pos.x
        TextBoxPosY.Text = LePersonnage(57).pos.y
        TextBoxCroyance.Text = LePersonnage(57).croyance
        TextBoxPersuasion.Text = LePersonnage(57).persuasion
        TextBoxCredulite.Text = LePersonnage(57).credulite
        TextBoxConvaincu.Text = LePersonnage(57).convaincu
        If TextBoxConvaincu.Text = "False" Then
            TextBoxConvaincu.Text = "Non"
        ElseIf TextBoxConvaincu.Text = "True" Then
            TextBoxConvaincu.Text = "Oui"
        End If
        For i = 0 To 99
            lesautrespicturebox = TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y)
            lesautrespicturebox.BorderStyle = BorderStyle.None
        Next
        PictureBox58.BorderStyle = BorderStyle.Fixed3D
        PictureBox102.BackColor = PictureBox58.BackColor
    End Sub

    Private Sub PictureBox59_Click(sender As Object, e As EventArgs) Handles PictureBox59.Click
        Dim lesautrespicturebox As PictureBox
        TextBoxPosX.Text = LePersonnage(58).pos.x
        TextBoxPosY.Text = LePersonnage(58).pos.y
        TextBoxCroyance.Text = LePersonnage(58).croyance
        TextBoxPersuasion.Text = LePersonnage(58).persuasion
        TextBoxCredulite.Text = LePersonnage(58).credulite
        TextBoxConvaincu.Text = LePersonnage(58).convaincu
        If TextBoxConvaincu.Text = "False" Then
            TextBoxConvaincu.Text = "Non"
        ElseIf TextBoxConvaincu.Text = "True" Then
            TextBoxConvaincu.Text = "Oui"
        End If
        For i = 0 To 99
            lesautrespicturebox = TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y)
            lesautrespicturebox.BorderStyle = BorderStyle.None
        Next
        PictureBox59.BorderStyle = BorderStyle.Fixed3D
        PictureBox102.BackColor = PictureBox59.BackColor
    End Sub

    Private Sub PictureBox60_Click(sender As Object, e As EventArgs) Handles PictureBox60.Click
        Dim lesautrespicturebox As PictureBox
        TextBoxPosX.Text = LePersonnage(59).pos.x
        TextBoxPosY.Text = LePersonnage(59).pos.y
        TextBoxCroyance.Text = LePersonnage(59).croyance
        TextBoxPersuasion.Text = LePersonnage(59).persuasion
        TextBoxCredulite.Text = LePersonnage(59).credulite
        TextBoxConvaincu.Text = LePersonnage(59).convaincu
        If TextBoxConvaincu.Text = "False" Then
            TextBoxConvaincu.Text = "Non"
        ElseIf TextBoxConvaincu.Text = "True" Then
            TextBoxConvaincu.Text = "Oui"
        End If
        For i = 0 To 99
            lesautrespicturebox = TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y)
            lesautrespicturebox.BorderStyle = BorderStyle.None
        Next
        PictureBox60.BorderStyle = BorderStyle.Fixed3D
        PictureBox102.BackColor = PictureBox60.BackColor
    End Sub

    Private Sub PictureBox61_Click(sender As Object, e As EventArgs) Handles PictureBox61.Click
        Dim lesautrespicturebox As PictureBox
        TextBoxPosX.Text = LePersonnage(60).pos.x
        TextBoxPosY.Text = LePersonnage(60).pos.y
        TextBoxCroyance.Text = LePersonnage(60).croyance
        TextBoxPersuasion.Text = LePersonnage(60).persuasion
        TextBoxCredulite.Text = LePersonnage(60).credulite
        TextBoxConvaincu.Text = LePersonnage(60).convaincu
        If TextBoxConvaincu.Text = "False" Then
            TextBoxConvaincu.Text = "Non"
        ElseIf TextBoxConvaincu.Text = "True" Then
            TextBoxConvaincu.Text = "Oui"
        End If
        For i = 0 To 99
            lesautrespicturebox = TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y)
            lesautrespicturebox.BorderStyle = BorderStyle.None
        Next
        PictureBox61.BorderStyle = BorderStyle.Fixed3D
        PictureBox102.BackColor = PictureBox61.BackColor
    End Sub

    Private Sub PictureBox62_Click(sender As Object, e As EventArgs) Handles PictureBox62.Click
        Dim lesautrespicturebox As PictureBox
        TextBoxPosX.Text = LePersonnage(61).pos.x
        TextBoxPosY.Text = LePersonnage(61).pos.y
        TextBoxCroyance.Text = LePersonnage(61).croyance
        TextBoxPersuasion.Text = LePersonnage(61).persuasion
        TextBoxCredulite.Text = LePersonnage(61).credulite
        TextBoxConvaincu.Text = LePersonnage(61).convaincu
        If TextBoxConvaincu.Text = "False" Then
            TextBoxConvaincu.Text = "Non"
        ElseIf TextBoxConvaincu.Text = "True" Then
            TextBoxConvaincu.Text = "Oui"
        End If
        For i = 0 To 99
            lesautrespicturebox = TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y)
            lesautrespicturebox.BorderStyle = BorderStyle.None
        Next
        PictureBox62.BorderStyle = BorderStyle.Fixed3D
        PictureBox102.BackColor = PictureBox62.BackColor
    End Sub

    Private Sub PictureBox63_Click(sender As Object, e As EventArgs) Handles PictureBox63.Click
        Dim lesautrespicturebox As PictureBox
        TextBoxPosX.Text = LePersonnage(62).pos.x
        TextBoxPosY.Text = LePersonnage(62).pos.y
        TextBoxCroyance.Text = LePersonnage(62).croyance
        TextBoxPersuasion.Text = LePersonnage(62).persuasion
        TextBoxCredulite.Text = LePersonnage(62).credulite
        TextBoxConvaincu.Text = LePersonnage(62).convaincu
        If TextBoxConvaincu.Text = "False" Then
            TextBoxConvaincu.Text = "Non"
        ElseIf TextBoxConvaincu.Text = "True" Then
            TextBoxConvaincu.Text = "Oui"
        End If
        For i = 0 To 99
            lesautrespicturebox = TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y)
            lesautrespicturebox.BorderStyle = BorderStyle.None
        Next
        PictureBox63.BorderStyle = BorderStyle.Fixed3D
        PictureBox102.BackColor = PictureBox63.BackColor
    End Sub

    Private Sub PictureBox64_Click(sender As Object, e As EventArgs) Handles PictureBox64.Click
        Dim lesautrespicturebox As PictureBox
        TextBoxPosX.Text = LePersonnage(63).pos.x
        TextBoxPosY.Text = LePersonnage(63).pos.y
        TextBoxCroyance.Text = LePersonnage(63).croyance
        TextBoxPersuasion.Text = LePersonnage(63).persuasion
        TextBoxCredulite.Text = LePersonnage(63).credulite
        TextBoxConvaincu.Text = LePersonnage(63).convaincu
        If TextBoxConvaincu.Text = "False" Then
            TextBoxConvaincu.Text = "Non"
        ElseIf TextBoxConvaincu.Text = "True" Then
            TextBoxConvaincu.Text = "Oui"
        End If
        For i = 0 To 99
            lesautrespicturebox = TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y)
            lesautrespicturebox.BorderStyle = BorderStyle.None
        Next
        PictureBox64.BorderStyle = BorderStyle.Fixed3D
        PictureBox102.BackColor = PictureBox64.BackColor
    End Sub

    Private Sub PictureBox65_Click(sender As Object, e As EventArgs) Handles PictureBox65.Click
        Dim lesautrespicturebox As PictureBox
        TextBoxPosX.Text = LePersonnage(64).pos.x
        TextBoxPosY.Text = LePersonnage(64).pos.y
        TextBoxCroyance.Text = LePersonnage(64).croyance
        TextBoxPersuasion.Text = LePersonnage(64).persuasion
        TextBoxCredulite.Text = LePersonnage(64).credulite
        TextBoxConvaincu.Text = LePersonnage(64).convaincu
        If TextBoxConvaincu.Text = "False" Then
            TextBoxConvaincu.Text = "Non"
        ElseIf TextBoxConvaincu.Text = "True" Then
            TextBoxConvaincu.Text = "Oui"
        End If
        For i = 0 To 99
            lesautrespicturebox = TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y)
            lesautrespicturebox.BorderStyle = BorderStyle.None
        Next
        PictureBox65.BorderStyle = BorderStyle.Fixed3D
        PictureBox102.BackColor = PictureBox65.BackColor
    End Sub

    Private Sub PictureBox66_Click(sender As Object, e As EventArgs) Handles PictureBox66.Click
        Dim lesautrespicturebox As PictureBox
        TextBoxPosX.Text = LePersonnage(65).pos.x
        TextBoxPosY.Text = LePersonnage(65).pos.y
        TextBoxCroyance.Text = LePersonnage(65).croyance
        TextBoxPersuasion.Text = LePersonnage(65).persuasion
        TextBoxCredulite.Text = LePersonnage(65).credulite
        TextBoxConvaincu.Text = LePersonnage(65).convaincu
        If TextBoxConvaincu.Text = "False" Then
            TextBoxConvaincu.Text = "Non"
        ElseIf TextBoxConvaincu.Text = "True" Then
            TextBoxConvaincu.Text = "Oui"
        End If
        For i = 0 To 99
            lesautrespicturebox = TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y)
            lesautrespicturebox.BorderStyle = BorderStyle.None
        Next
        PictureBox66.BorderStyle = BorderStyle.Fixed3D
        PictureBox102.BackColor = PictureBox66.BackColor
    End Sub

    Private Sub PictureBox67_Click(sender As Object, e As EventArgs) Handles PictureBox67.Click
        Dim lesautrespicturebox As PictureBox
        TextBoxPosX.Text = LePersonnage(66).pos.x
        TextBoxPosY.Text = LePersonnage(66).pos.y
        TextBoxCroyance.Text = LePersonnage(66).croyance
        TextBoxPersuasion.Text = LePersonnage(66).persuasion
        TextBoxCredulite.Text = LePersonnage(66).credulite
        TextBoxConvaincu.Text = LePersonnage(66).convaincu
        If TextBoxConvaincu.Text = "False" Then
            TextBoxConvaincu.Text = "Non"
        ElseIf TextBoxConvaincu.Text = "True" Then
            TextBoxConvaincu.Text = "Oui"
        End If
        For i = 0 To 99
            lesautrespicturebox = TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y)
            lesautrespicturebox.BorderStyle = BorderStyle.None
        Next
        PictureBox67.BorderStyle = BorderStyle.Fixed3D
        PictureBox102.BackColor = PictureBox67.BackColor
    End Sub

    Private Sub PictureBox68_Click(sender As Object, e As EventArgs) Handles PictureBox68.Click
        Dim lesautrespicturebox As PictureBox
        TextBoxPosX.Text = LePersonnage(67).pos.x
        TextBoxPosY.Text = LePersonnage(67).pos.y
        TextBoxCroyance.Text = LePersonnage(67).croyance
        TextBoxPersuasion.Text = LePersonnage(67).persuasion
        TextBoxCredulite.Text = LePersonnage(67).credulite
        TextBoxConvaincu.Text = LePersonnage(67).convaincu
        If TextBoxConvaincu.Text = "False" Then
            TextBoxConvaincu.Text = "Non"
        ElseIf TextBoxConvaincu.Text = "True" Then
            TextBoxConvaincu.Text = "Oui"
        End If
        For i = 0 To 99
            lesautrespicturebox = TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y)
            lesautrespicturebox.BorderStyle = BorderStyle.None
        Next
        PictureBox68.BorderStyle = BorderStyle.Fixed3D
        PictureBox102.BackColor = PictureBox68.BackColor
    End Sub

    Private Sub PictureBox69_Click(sender As Object, e As EventArgs) Handles PictureBox69.Click
        Dim lesautrespicturebox As PictureBox
        TextBoxPosX.Text = LePersonnage(68).pos.x
        TextBoxPosY.Text = LePersonnage(68).pos.y
        TextBoxCroyance.Text = LePersonnage(68).croyance
        TextBoxPersuasion.Text = LePersonnage(68).persuasion
        TextBoxCredulite.Text = LePersonnage(68).credulite
        TextBoxConvaincu.Text = LePersonnage(68).convaincu
        If TextBoxConvaincu.Text = "False" Then
            TextBoxConvaincu.Text = "Non"
        ElseIf TextBoxConvaincu.Text = "True" Then
            TextBoxConvaincu.Text = "Oui"
        End If
        For i = 0 To 99
            lesautrespicturebox = TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y)
            lesautrespicturebox.BorderStyle = BorderStyle.None
        Next
        PictureBox69.BorderStyle = BorderStyle.Fixed3D
        PictureBox102.BackColor = PictureBox69.BackColor
    End Sub

    Private Sub PictureBox70_Click(sender As Object, e As EventArgs) Handles PictureBox70.Click
        Dim lesautrespicturebox As PictureBox
        TextBoxPosX.Text = LePersonnage(69).pos.x
        TextBoxPosY.Text = LePersonnage(69).pos.y
        TextBoxCroyance.Text = LePersonnage(69).croyance
        TextBoxPersuasion.Text = LePersonnage(69).persuasion
        TextBoxCredulite.Text = LePersonnage(69).credulite
        TextBoxConvaincu.Text = LePersonnage(69).convaincu
        If TextBoxConvaincu.Text = "False" Then
            TextBoxConvaincu.Text = "Non"
        ElseIf TextBoxConvaincu.Text = "True" Then
            TextBoxConvaincu.Text = "Oui"
        End If
        For i = 0 To 99
            lesautrespicturebox = TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y)
            lesautrespicturebox.BorderStyle = BorderStyle.None
        Next
        PictureBox70.BorderStyle = BorderStyle.Fixed3D
        PictureBox102.BackColor = PictureBox70.BackColor
    End Sub

    Private Sub PictureBox71_Click(sender As Object, e As EventArgs) Handles PictureBox71.Click
        Dim lesautrespicturebox As PictureBox
        TextBoxPosX.Text = LePersonnage(70).pos.x
        TextBoxPosY.Text = LePersonnage(70).pos.y
        TextBoxCroyance.Text = LePersonnage(70).croyance
        TextBoxPersuasion.Text = LePersonnage(70).persuasion
        TextBoxCredulite.Text = LePersonnage(70).credulite
        TextBoxConvaincu.Text = LePersonnage(70).convaincu
        If TextBoxConvaincu.Text = "False" Then
            TextBoxConvaincu.Text = "Non"
        ElseIf TextBoxConvaincu.Text = "True" Then
            TextBoxConvaincu.Text = "Oui"
        End If
        For i = 0 To 99
            lesautrespicturebox = TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y)
            lesautrespicturebox.BorderStyle = BorderStyle.None
        Next
        PictureBox71.BorderStyle = BorderStyle.Fixed3D
        PictureBox102.BackColor = PictureBox71.BackColor
    End Sub

    Private Sub PictureBox72_Click(sender As Object, e As EventArgs) Handles PictureBox72.Click
        Dim lesautrespicturebox As PictureBox
        TextBoxPosX.Text = LePersonnage(71).pos.x
        TextBoxPosY.Text = LePersonnage(71).pos.y
        TextBoxCroyance.Text = LePersonnage(71).croyance
        TextBoxPersuasion.Text = LePersonnage(71).persuasion
        TextBoxCredulite.Text = LePersonnage(71).credulite
        TextBoxConvaincu.Text = LePersonnage(71).convaincu
        If TextBoxConvaincu.Text = "False" Then
            TextBoxConvaincu.Text = "Non"
        ElseIf TextBoxConvaincu.Text = "True" Then
            TextBoxConvaincu.Text = "Oui"
        End If
        For i = 0 To 99
            lesautrespicturebox = TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y)
            lesautrespicturebox.BorderStyle = BorderStyle.None
        Next
        PictureBox72.BorderStyle = BorderStyle.Fixed3D
        PictureBox102.BackColor = PictureBox72.BackColor
    End Sub

    Private Sub PictureBox73_Click(sender As Object, e As EventArgs) Handles PictureBox73.Click
        Dim lesautrespicturebox As PictureBox
        TextBoxPosX.Text = LePersonnage(72).pos.x
        TextBoxPosY.Text = LePersonnage(72).pos.y
        TextBoxCroyance.Text = LePersonnage(72).croyance
        TextBoxPersuasion.Text = LePersonnage(72).persuasion
        TextBoxCredulite.Text = LePersonnage(72).credulite
        TextBoxConvaincu.Text = LePersonnage(72).convaincu
        If TextBoxConvaincu.Text = "False" Then
            TextBoxConvaincu.Text = "Non"
        ElseIf TextBoxConvaincu.Text = "True" Then
            TextBoxConvaincu.Text = "Oui"
        End If
        For i = 0 To 99
            lesautrespicturebox = TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y)
            lesautrespicturebox.BorderStyle = BorderStyle.None
        Next
        PictureBox73.BorderStyle = BorderStyle.Fixed3D
        PictureBox102.BackColor = PictureBox73.BackColor
    End Sub

    Private Sub PictureBox74_Click(sender As Object, e As EventArgs) Handles PictureBox74.Click
        Dim lesautrespicturebox As PictureBox
        TextBoxPosX.Text = LePersonnage(73).pos.x
        TextBoxPosY.Text = LePersonnage(73).pos.y
        TextBoxCroyance.Text = LePersonnage(73).croyance
        TextBoxPersuasion.Text = LePersonnage(73).persuasion
        TextBoxCredulite.Text = LePersonnage(73).credulite
        TextBoxConvaincu.Text = LePersonnage(73).convaincu
        If TextBoxConvaincu.Text = "False" Then
            TextBoxConvaincu.Text = "Non"
        ElseIf TextBoxConvaincu.Text = "True" Then
            TextBoxConvaincu.Text = "Oui"
        End If
        For i = 0 To 99
            lesautrespicturebox = TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y)
            lesautrespicturebox.BorderStyle = BorderStyle.None
        Next
        PictureBox74.BorderStyle = BorderStyle.Fixed3D
        PictureBox102.BackColor = PictureBox74.BackColor
    End Sub

    Private Sub PictureBox75_Click(sender As Object, e As EventArgs) Handles PictureBox75.Click
        Dim lesautrespicturebox As PictureBox
        TextBoxPosX.Text = LePersonnage(74).pos.x
        TextBoxPosY.Text = LePersonnage(74).pos.y
        TextBoxCroyance.Text = LePersonnage(74).croyance
        TextBoxPersuasion.Text = LePersonnage(74).persuasion
        TextBoxCredulite.Text = LePersonnage(74).credulite
        TextBoxConvaincu.Text = LePersonnage(74).convaincu
        If TextBoxConvaincu.Text = "False" Then
            TextBoxConvaincu.Text = "Non"
        ElseIf TextBoxConvaincu.Text = "True" Then
            TextBoxConvaincu.Text = "Oui"
        End If
        For i = 0 To 99
            lesautrespicturebox = TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y)
            lesautrespicturebox.BorderStyle = BorderStyle.None
        Next
        PictureBox75.BorderStyle = BorderStyle.Fixed3D
        PictureBox102.BackColor = PictureBox75.BackColor
    End Sub

    Private Sub PictureBox76_Click(sender As Object, e As EventArgs) Handles PictureBox76.Click
        Dim lesautrespicturebox As PictureBox
        TextBoxPosX.Text = LePersonnage(75).pos.x
        TextBoxPosY.Text = LePersonnage(75).pos.y
        TextBoxCroyance.Text = LePersonnage(75).croyance
        TextBoxPersuasion.Text = LePersonnage(75).persuasion
        TextBoxCredulite.Text = LePersonnage(75).credulite
        TextBoxConvaincu.Text = LePersonnage(75).convaincu
        If TextBoxConvaincu.Text = "False" Then
            TextBoxConvaincu.Text = "Non"
        ElseIf TextBoxConvaincu.Text = "True" Then
            TextBoxConvaincu.Text = "Oui"
        End If
        For i = 0 To 99
            lesautrespicturebox = TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y)
            lesautrespicturebox.BorderStyle = BorderStyle.None
        Next
        PictureBox76.BorderStyle = BorderStyle.Fixed3D
        PictureBox102.BackColor = PictureBox76.BackColor
    End Sub

    Private Sub PictureBox77_Click(sender As Object, e As EventArgs) Handles PictureBox77.Click
        Dim lesautrespicturebox As PictureBox
        TextBoxPosX.Text = LePersonnage(76).pos.x
        TextBoxPosY.Text = LePersonnage(76).pos.y
        TextBoxCroyance.Text = LePersonnage(76).croyance
        TextBoxPersuasion.Text = LePersonnage(76).persuasion
        TextBoxCredulite.Text = LePersonnage(76).credulite
        TextBoxConvaincu.Text = LePersonnage(76).convaincu
        If TextBoxConvaincu.Text = "False" Then
            TextBoxConvaincu.Text = "Non"
        ElseIf TextBoxConvaincu.Text = "True" Then
            TextBoxConvaincu.Text = "Oui"
        End If
        For i = 0 To 99
            lesautrespicturebox = TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y)
            lesautrespicturebox.BorderStyle = BorderStyle.None
        Next
        PictureBox77.BorderStyle = BorderStyle.Fixed3D
        PictureBox102.BackColor = PictureBox77.BackColor
    End Sub

    Private Sub PictureBox78_Click(sender As Object, e As EventArgs) Handles PictureBox78.Click
        Dim lesautrespicturebox As PictureBox
        TextBoxPosX.Text = LePersonnage(77).pos.x
        TextBoxPosY.Text = LePersonnage(77).pos.y
        TextBoxCroyance.Text = LePersonnage(77).croyance
        TextBoxPersuasion.Text = LePersonnage(77).persuasion
        TextBoxCredulite.Text = LePersonnage(77).credulite
        TextBoxConvaincu.Text = LePersonnage(77).convaincu
        If TextBoxConvaincu.Text = "False" Then
            TextBoxConvaincu.Text = "Non"
        ElseIf TextBoxConvaincu.Text = "True" Then
            TextBoxConvaincu.Text = "Oui"
        End If
        For i = 0 To 99
            lesautrespicturebox = TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y)
            lesautrespicturebox.BorderStyle = BorderStyle.None
        Next
        PictureBox78.BorderStyle = BorderStyle.Fixed3D
        PictureBox102.BackColor = PictureBox78.BackColor
    End Sub

    Private Sub PictureBox79_Click(sender As Object, e As EventArgs) Handles PictureBox79.Click
        Dim lesautrespicturebox As PictureBox
        TextBoxPosX.Text = LePersonnage(78).pos.x
        TextBoxPosY.Text = LePersonnage(78).pos.y
        TextBoxCroyance.Text = LePersonnage(78).croyance
        TextBoxPersuasion.Text = LePersonnage(78).persuasion
        TextBoxCredulite.Text = LePersonnage(78).credulite
        TextBoxConvaincu.Text = LePersonnage(78).convaincu
        If TextBoxConvaincu.Text = "False" Then
            TextBoxConvaincu.Text = "Non"
        ElseIf TextBoxConvaincu.Text = "True" Then
            TextBoxConvaincu.Text = "Oui"
        End If
        For i = 0 To 99
            lesautrespicturebox = TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y)
            lesautrespicturebox.BorderStyle = BorderStyle.None
        Next
        PictureBox79.BorderStyle = BorderStyle.Fixed3D
        PictureBox102.BackColor = PictureBox79.BackColor
    End Sub

    Private Sub PictureBox80_Click(sender As Object, e As EventArgs) Handles PictureBox80.Click
        Dim lesautrespicturebox As PictureBox
        TextBoxPosX.Text = LePersonnage(79).pos.x
        TextBoxPosY.Text = LePersonnage(79).pos.y
        TextBoxCroyance.Text = LePersonnage(79).croyance
        TextBoxPersuasion.Text = LePersonnage(79).persuasion
        TextBoxCredulite.Text = LePersonnage(79).credulite
        TextBoxConvaincu.Text = LePersonnage(79).convaincu
        If TextBoxConvaincu.Text = "False" Then
            TextBoxConvaincu.Text = "Non"
        ElseIf TextBoxConvaincu.Text = "True" Then
            TextBoxConvaincu.Text = "Oui"
        End If
        For i = 0 To 99
            lesautrespicturebox = TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y)
            lesautrespicturebox.BorderStyle = BorderStyle.None
        Next
        PictureBox80.BorderStyle = BorderStyle.Fixed3D
        PictureBox102.BackColor = PictureBox80.BackColor
    End Sub

    Private Sub PictureBox81_Click(sender As Object, e As EventArgs) Handles PictureBox81.Click
        Dim lesautrespicturebox As PictureBox
        TextBoxPosX.Text = LePersonnage(80).pos.x
        TextBoxPosY.Text = LePersonnage(80).pos.y
        TextBoxCroyance.Text = LePersonnage(80).croyance
        TextBoxPersuasion.Text = LePersonnage(80).persuasion
        TextBoxCredulite.Text = LePersonnage(80).credulite
        TextBoxConvaincu.Text = LePersonnage(80).convaincu
        If TextBoxConvaincu.Text = "False" Then
            TextBoxConvaincu.Text = "Non"
        ElseIf TextBoxConvaincu.Text = "True" Then
            TextBoxConvaincu.Text = "Oui"
        End If
        For i = 0 To 99
            lesautrespicturebox = TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y)
            lesautrespicturebox.BorderStyle = BorderStyle.None
        Next
        PictureBox81.BorderStyle = BorderStyle.Fixed3D
        PictureBox102.BackColor = PictureBox81.BackColor
    End Sub

    Private Sub PictureBox82_Click(sender As Object, e As EventArgs) Handles PictureBox82.Click
        Dim lesautrespicturebox As PictureBox
        TextBoxPosX.Text = LePersonnage(81).pos.x
        TextBoxPosY.Text = LePersonnage(81).pos.y
        TextBoxCroyance.Text = LePersonnage(81).croyance
        TextBoxPersuasion.Text = LePersonnage(81).persuasion
        TextBoxCredulite.Text = LePersonnage(81).credulite
        TextBoxConvaincu.Text = LePersonnage(81).convaincu
        If TextBoxConvaincu.Text = "False" Then
            TextBoxConvaincu.Text = "Non"
        ElseIf TextBoxConvaincu.Text = "True" Then
            TextBoxConvaincu.Text = "Oui"
        End If
        For i = 0 To 99
            lesautrespicturebox = TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y)
            lesautrespicturebox.BorderStyle = BorderStyle.None
        Next
        PictureBox82.BorderStyle = BorderStyle.Fixed3D
        PictureBox102.BackColor = PictureBox82.BackColor
    End Sub

    Private Sub PictureBox83_Click(sender As Object, e As EventArgs) Handles PictureBox83.Click
        Dim lesautrespicturebox As PictureBox
        TextBoxPosX.Text = LePersonnage(82).pos.x
        TextBoxPosY.Text = LePersonnage(82).pos.y
        TextBoxCroyance.Text = LePersonnage(82).croyance
        TextBoxPersuasion.Text = LePersonnage(82).persuasion
        TextBoxCredulite.Text = LePersonnage(82).credulite
        TextBoxConvaincu.Text = LePersonnage(82).convaincu
        If TextBoxConvaincu.Text = "False" Then
            TextBoxConvaincu.Text = "Non"
        ElseIf TextBoxConvaincu.Text = "True" Then
            TextBoxConvaincu.Text = "Oui"
        End If
        For i = 0 To 99
            lesautrespicturebox = TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y)
            lesautrespicturebox.BorderStyle = BorderStyle.None
        Next
        PictureBox83.BorderStyle = BorderStyle.Fixed3D
        PictureBox102.BackColor = PictureBox83.BackColor
    End Sub

    Private Sub PictureBox84_Click(sender As Object, e As EventArgs) Handles PictureBox84.Click
        Dim lesautrespicturebox As PictureBox
        TextBoxPosX.Text = LePersonnage(83).pos.x
        TextBoxPosY.Text = LePersonnage(83).pos.y
        TextBoxCroyance.Text = LePersonnage(83).croyance
        TextBoxPersuasion.Text = LePersonnage(83).persuasion
        TextBoxCredulite.Text = LePersonnage(83).credulite
        TextBoxConvaincu.Text = LePersonnage(83).convaincu
        If TextBoxConvaincu.Text = "False" Then
            TextBoxConvaincu.Text = "Non"
        ElseIf TextBoxConvaincu.Text = "True" Then
            TextBoxConvaincu.Text = "Oui"
        End If
        For i = 0 To 99
            lesautrespicturebox = TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y)
            lesautrespicturebox.BorderStyle = BorderStyle.None
        Next
        PictureBox84.BorderStyle = BorderStyle.Fixed3D
        PictureBox102.BackColor = PictureBox84.BackColor
    End Sub

    Private Sub PictureBox85_Click(sender As Object, e As EventArgs) Handles PictureBox85.Click
        Dim lesautrespicturebox As PictureBox
        TextBoxPosX.Text = LePersonnage(84).pos.x
        TextBoxPosY.Text = LePersonnage(84).pos.y
        TextBoxCroyance.Text = LePersonnage(84).croyance
        TextBoxPersuasion.Text = LePersonnage(84).persuasion
        TextBoxCredulite.Text = LePersonnage(84).credulite
        TextBoxConvaincu.Text = LePersonnage(84).convaincu
        If TextBoxConvaincu.Text = "False" Then
            TextBoxConvaincu.Text = "Non"
        ElseIf TextBoxConvaincu.Text = "True" Then
            TextBoxConvaincu.Text = "Oui"
        End If
        For i = 0 To 99
            lesautrespicturebox = TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y)
            lesautrespicturebox.BorderStyle = BorderStyle.None
        Next
        PictureBox85.BorderStyle = BorderStyle.Fixed3D
        PictureBox102.BackColor = PictureBox85.BackColor
    End Sub

    Private Sub PictureBox86_Click(sender As Object, e As EventArgs) Handles PictureBox86.Click
        Dim lesautrespicturebox As PictureBox
        TextBoxPosX.Text = LePersonnage(85).pos.x
        TextBoxPosY.Text = LePersonnage(85).pos.y
        TextBoxCroyance.Text = LePersonnage(85).croyance
        TextBoxPersuasion.Text = LePersonnage(85).persuasion
        TextBoxCredulite.Text = LePersonnage(85).credulite
        TextBoxConvaincu.Text = LePersonnage(85).convaincu
        If TextBoxConvaincu.Text = "False" Then
            TextBoxConvaincu.Text = "Non"
        ElseIf TextBoxConvaincu.Text = "True" Then
            TextBoxConvaincu.Text = "Oui"
        End If
        For i = 0 To 99
            lesautrespicturebox = TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y)
            lesautrespicturebox.BorderStyle = BorderStyle.None
        Next
        PictureBox86.BorderStyle = BorderStyle.Fixed3D
        PictureBox102.BackColor = PictureBox86.BackColor
    End Sub

    Private Sub PictureBox87_Click(sender As Object, e As EventArgs) Handles PictureBox87.Click
        Dim lesautrespicturebox As PictureBox
        TextBoxPosX.Text = LePersonnage(86).pos.x
        TextBoxPosY.Text = LePersonnage(86).pos.y
        TextBoxCroyance.Text = LePersonnage(86).croyance
        TextBoxPersuasion.Text = LePersonnage(86).persuasion
        TextBoxCredulite.Text = LePersonnage(86).credulite
        TextBoxConvaincu.Text = LePersonnage(86).convaincu
        If TextBoxConvaincu.Text = "False" Then
            TextBoxConvaincu.Text = "Non"
        ElseIf TextBoxConvaincu.Text = "True" Then
            TextBoxConvaincu.Text = "Oui"
        End If
        For i = 0 To 99
            lesautrespicturebox = TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y)
            lesautrespicturebox.BorderStyle = BorderStyle.None
        Next
        PictureBox87.BorderStyle = BorderStyle.Fixed3D
        PictureBox102.BackColor = PictureBox87.BackColor
    End Sub

    Private Sub PictureBox88_Click(sender As Object, e As EventArgs) Handles PictureBox88.Click
        Dim lesautrespicturebox As PictureBox
        TextBoxPosX.Text = LePersonnage(87).pos.x
        TextBoxPosY.Text = LePersonnage(87).pos.y
        TextBoxCroyance.Text = LePersonnage(87).croyance
        TextBoxPersuasion.Text = LePersonnage(87).persuasion
        TextBoxCredulite.Text = LePersonnage(87).credulite
        TextBoxConvaincu.Text = LePersonnage(87).convaincu
        If TextBoxConvaincu.Text = "False" Then
            TextBoxConvaincu.Text = "Non"
        ElseIf TextBoxConvaincu.Text = "True" Then
            TextBoxConvaincu.Text = "Oui"
        End If
        For i = 0 To 99
            lesautrespicturebox = TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y)
            lesautrespicturebox.BorderStyle = BorderStyle.None
        Next
        PictureBox88.BorderStyle = BorderStyle.Fixed3D
        PictureBox102.BackColor = PictureBox88.BackColor
    End Sub

    Private Sub PictureBox89_Click(sender As Object, e As EventArgs) Handles PictureBox89.Click
        Dim lesautrespicturebox As PictureBox
        TextBoxPosX.Text = LePersonnage(88).pos.x
        TextBoxPosY.Text = LePersonnage(88).pos.y
        TextBoxCroyance.Text = LePersonnage(88).croyance
        TextBoxPersuasion.Text = LePersonnage(88).persuasion
        TextBoxCredulite.Text = LePersonnage(88).credulite
        TextBoxConvaincu.Text = LePersonnage(88).convaincu
        If TextBoxConvaincu.Text = "False" Then
            TextBoxConvaincu.Text = "Non"
        ElseIf TextBoxConvaincu.Text = "True" Then
            TextBoxConvaincu.Text = "Oui"
        End If
        For i = 0 To 99
            lesautrespicturebox = TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y)
            lesautrespicturebox.BorderStyle = BorderStyle.None
        Next
        PictureBox89.BorderStyle = BorderStyle.Fixed3D
        PictureBox102.BackColor = PictureBox89.BackColor
    End Sub

    Private Sub PictureBox90_Click(sender As Object, e As EventArgs) Handles PictureBox90.Click
        Dim lesautrespicturebox As PictureBox
        TextBoxPosX.Text = LePersonnage(89).pos.x
        TextBoxPosY.Text = LePersonnage(89).pos.y
        TextBoxCroyance.Text = LePersonnage(89).croyance
        TextBoxPersuasion.Text = LePersonnage(89).persuasion
        TextBoxCredulite.Text = LePersonnage(89).credulite
        TextBoxConvaincu.Text = LePersonnage(89).convaincu
        If TextBoxConvaincu.Text = "False" Then
            TextBoxConvaincu.Text = "Non"
        ElseIf TextBoxConvaincu.Text = "True" Then
            TextBoxConvaincu.Text = "Oui"
        End If
        For i = 0 To 99
            lesautrespicturebox = TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y)
            lesautrespicturebox.BorderStyle = BorderStyle.None
        Next
        PictureBox90.BorderStyle = BorderStyle.Fixed3D
        PictureBox102.BackColor = PictureBox90.BackColor
    End Sub

    Private Sub PictureBox91_Click(sender As Object, e As EventArgs) Handles PictureBox91.Click
        Dim lesautrespicturebox As PictureBox
        TextBoxPosX.Text = LePersonnage(90).pos.x
        TextBoxPosY.Text = LePersonnage(90).pos.y
        TextBoxCroyance.Text = LePersonnage(90).croyance
        TextBoxPersuasion.Text = LePersonnage(90).persuasion
        TextBoxCredulite.Text = LePersonnage(90).credulite
        TextBoxConvaincu.Text = LePersonnage(90).convaincu
        If TextBoxConvaincu.Text = "False" Then
            TextBoxConvaincu.Text = "Non"
        ElseIf TextBoxConvaincu.Text = "True" Then
            TextBoxConvaincu.Text = "Oui"
        End If
        For i = 0 To 99
            lesautrespicturebox = TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y)
            lesautrespicturebox.BorderStyle = BorderStyle.None
        Next
        PictureBox91.BorderStyle = BorderStyle.Fixed3D
        PictureBox102.BackColor = PictureBox91.BackColor
    End Sub

    Private Sub PictureBox92_Click(sender As Object, e As EventArgs) Handles PictureBox92.Click
        Dim lesautrespicturebox As PictureBox
        TextBoxPosX.Text = LePersonnage(91).pos.x
        TextBoxPosY.Text = LePersonnage(91).pos.y
        TextBoxCroyance.Text = LePersonnage(91).croyance
        TextBoxPersuasion.Text = LePersonnage(91).persuasion
        TextBoxCredulite.Text = LePersonnage(91).credulite
        TextBoxConvaincu.Text = LePersonnage(91).convaincu
        If TextBoxConvaincu.Text = "False" Then
            TextBoxConvaincu.Text = "Non"
        ElseIf TextBoxConvaincu.Text = "True" Then
            TextBoxConvaincu.Text = "Oui"
        End If
        For i = 0 To 99
            lesautrespicturebox = TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y)
            lesautrespicturebox.BorderStyle = BorderStyle.None
        Next
        PictureBox92.BorderStyle = BorderStyle.Fixed3D
        PictureBox102.BackColor = PictureBox92.BackColor
    End Sub

    Private Sub PictureBox93_Click(sender As Object, e As EventArgs) Handles PictureBox93.Click
        Dim lesautrespicturebox As PictureBox
        TextBoxPosX.Text = LePersonnage(92).pos.x
        TextBoxPosY.Text = LePersonnage(92).pos.y
        TextBoxCroyance.Text = LePersonnage(92).croyance
        TextBoxPersuasion.Text = LePersonnage(92).persuasion
        TextBoxCredulite.Text = LePersonnage(92).credulite
        TextBoxConvaincu.Text = LePersonnage(92).convaincu
        If TextBoxConvaincu.Text = "False" Then
            TextBoxConvaincu.Text = "Non"
        ElseIf TextBoxConvaincu.Text = "True" Then
            TextBoxConvaincu.Text = "Oui"
        End If
        For i = 0 To 99
            lesautrespicturebox = TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y)
            lesautrespicturebox.BorderStyle = BorderStyle.None
        Next
        PictureBox93.BorderStyle = BorderStyle.Fixed3D
        PictureBox102.BackColor = PictureBox93.BackColor
    End Sub

    Private Sub PictureBox94_Click(sender As Object, e As EventArgs) Handles PictureBox94.Click
        Dim lesautrespicturebox As PictureBox
        TextBoxPosX.Text = LePersonnage(93).pos.x
        TextBoxPosY.Text = LePersonnage(93).pos.y
        TextBoxCroyance.Text = LePersonnage(93).croyance
        TextBoxPersuasion.Text = LePersonnage(93).persuasion
        TextBoxCredulite.Text = LePersonnage(93).credulite
        TextBoxConvaincu.Text = LePersonnage(93).convaincu
        If TextBoxConvaincu.Text = "False" Then
            TextBoxConvaincu.Text = "Non"
        ElseIf TextBoxConvaincu.Text = "True" Then
            TextBoxConvaincu.Text = "Oui"
        End If
        For i = 0 To 99
            lesautrespicturebox = TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y)
            lesautrespicturebox.BorderStyle = BorderStyle.None
        Next
        PictureBox94.BorderStyle = BorderStyle.Fixed3D
        PictureBox102.BackColor = PictureBox94.BackColor
    End Sub

    Private Sub PictureBox95_Click(sender As Object, e As EventArgs) Handles PictureBox95.Click
        Dim lesautrespicturebox As PictureBox
        TextBoxPosX.Text = LePersonnage(94).pos.x
        TextBoxPosY.Text = LePersonnage(94).pos.y
        TextBoxCroyance.Text = LePersonnage(94).croyance
        TextBoxPersuasion.Text = LePersonnage(94).persuasion
        TextBoxCredulite.Text = LePersonnage(94).credulite
        TextBoxConvaincu.Text = LePersonnage(94).convaincu
        If TextBoxConvaincu.Text = "False" Then
            TextBoxConvaincu.Text = "Non"
        ElseIf TextBoxConvaincu.Text = "True" Then
            TextBoxConvaincu.Text = "Oui"
        End If
        For i = 0 To 99
            lesautrespicturebox = TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y)
            lesautrespicturebox.BorderStyle = BorderStyle.None
        Next
        PictureBox95.BorderStyle = BorderStyle.Fixed3D
        PictureBox102.BackColor = PictureBox95.BackColor
    End Sub

    Private Sub PictureBox96_Click(sender As Object, e As EventArgs) Handles PictureBox96.Click
        Dim lesautrespicturebox As PictureBox
        TextBoxPosX.Text = LePersonnage(95).pos.x
        TextBoxPosY.Text = LePersonnage(95).pos.y
        TextBoxCroyance.Text = LePersonnage(95).croyance
        TextBoxPersuasion.Text = LePersonnage(95).persuasion
        TextBoxCredulite.Text = LePersonnage(95).credulite
        TextBoxConvaincu.Text = LePersonnage(95).convaincu
        If TextBoxConvaincu.Text = "False" Then
            TextBoxConvaincu.Text = "Non"
        ElseIf TextBoxConvaincu.Text = "True" Then
            TextBoxConvaincu.Text = "Oui"
        End If
        For i = 0 To 99
            lesautrespicturebox = TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y)
            lesautrespicturebox.BorderStyle = BorderStyle.None
        Next
        PictureBox96.BorderStyle = BorderStyle.Fixed3D
        PictureBox102.BackColor = PictureBox96.BackColor
    End Sub

    Private Sub PictureBox97_Click(sender As Object, e As EventArgs) Handles PictureBox97.Click
        Dim lesautrespicturebox As PictureBox
        TextBoxPosX.Text = LePersonnage(96).pos.x
        TextBoxPosY.Text = LePersonnage(96).pos.y
        TextBoxCroyance.Text = LePersonnage(96).croyance
        TextBoxPersuasion.Text = LePersonnage(96).persuasion
        TextBoxCredulite.Text = LePersonnage(96).credulite
        TextBoxConvaincu.Text = LePersonnage(96).convaincu
        If TextBoxConvaincu.Text = "False" Then
            TextBoxConvaincu.Text = "Non"
        ElseIf TextBoxConvaincu.Text = "True" Then
            TextBoxConvaincu.Text = "Oui"
        End If
        For i = 0 To 99
            lesautrespicturebox = TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y)
            lesautrespicturebox.BorderStyle = BorderStyle.None
        Next
        PictureBox97.BorderStyle = BorderStyle.Fixed3D
        PictureBox102.BackColor = PictureBox97.BackColor
    End Sub

    Private Sub PictureBox98_Click(sender As Object, e As EventArgs) Handles PictureBox98.Click
        Dim lesautrespicturebox As PictureBox
        TextBoxPosX.Text = LePersonnage(97).pos.x
        TextBoxPosY.Text = LePersonnage(97).pos.y
        TextBoxCroyance.Text = LePersonnage(97).croyance
        TextBoxPersuasion.Text = LePersonnage(97).persuasion
        TextBoxCredulite.Text = LePersonnage(97).credulite
        TextBoxConvaincu.Text = LePersonnage(97).convaincu
        If TextBoxConvaincu.Text = "False" Then
            TextBoxConvaincu.Text = "Non"
        ElseIf TextBoxConvaincu.Text = "True" Then
            TextBoxConvaincu.Text = "Oui"
        End If
        For i = 0 To 99
            lesautrespicturebox = TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y)
            lesautrespicturebox.BorderStyle = BorderStyle.None
        Next
        PictureBox98.BorderStyle = BorderStyle.Fixed3D
        PictureBox102.BackColor = PictureBox98.BackColor
    End Sub

    Private Sub PictureBox99_Click(sender As Object, e As EventArgs) Handles PictureBox99.Click
        Dim lesautrespicturebox As PictureBox
        TextBoxPosX.Text = LePersonnage(98).pos.x
        TextBoxPosY.Text = LePersonnage(98).pos.y
        TextBoxCroyance.Text = LePersonnage(98).croyance
        TextBoxPersuasion.Text = LePersonnage(98).persuasion
        TextBoxCredulite.Text = LePersonnage(98).credulite
        TextBoxConvaincu.Text = LePersonnage(98).convaincu
        If TextBoxConvaincu.Text = "False" Then
            TextBoxConvaincu.Text = "Non"
        ElseIf TextBoxConvaincu.Text = "True" Then
            TextBoxConvaincu.Text = "Oui"
        End If
        For i = 0 To 99
            lesautrespicturebox = TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y)
            lesautrespicturebox.BorderStyle = BorderStyle.None
        Next
        PictureBox99.BorderStyle = BorderStyle.Fixed3D
        PictureBox102.BackColor = PictureBox99.BackColor
    End Sub

    Private Sub PictureBox100_Click(sender As Object, e As EventArgs) Handles PictureBox100.Click
        Dim lesautrespicturebox As PictureBox
        TextBoxPosX.Text = LePersonnage(99).pos.x
        TextBoxPosY.Text = LePersonnage(99).pos.y
        TextBoxCroyance.Text = LePersonnage(99).croyance
        TextBoxPersuasion.Text = LePersonnage(99).persuasion
        TextBoxCredulite.Text = LePersonnage(99).credulite
        TextBoxConvaincu.Text = LePersonnage(99).convaincu
        If TextBoxConvaincu.Text = "False" Then
            TextBoxConvaincu.Text = "Non"
        ElseIf TextBoxConvaincu.Text = "True" Then
            TextBoxConvaincu.Text = "Oui"
        End If
        For i = 0 To 99
            lesautrespicturebox = TableLayoutPanel1.GetControlFromPosition(LePersonnage(i).pos.x, LePersonnage(i).pos.y)
            lesautrespicturebox.BorderStyle = BorderStyle.None
        Next
        PictureBox100.BorderStyle = BorderStyle.Fixed3D
        PictureBox102.BackColor = PictureBox100.BackColor
    End Sub


End Class








