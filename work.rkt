void automaton(bool cond1, bool cond2, bool cond3)
{
    if(cond1)
    {
        if(cond2)
        {
            if(cond3)
            {
                goto SthUseful;
            }
            else
            {
                throw ".....\n";
            }
        }
        else
        {
            cerr << "....\n";
            goto SthUseful;
        }
    }
    else
    {
        cerr << "...\n";
        goto SthUseful;
    }
}